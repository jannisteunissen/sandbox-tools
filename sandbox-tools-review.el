;;; sandbox-tools-review.el --- Review and apply sandbox changes -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna

;;; Commentary:

;; Commands to show, apply and discard what sandboxed commands wrote to
;; the project overlay, plus status, interrupt and a menu.
;;
;; Applying happens in two phases, so the real project is never written
;; while it is mounted as an overlay layer:
;;   1. inside bwrap, rsync the merged overlay to a staging directory;
;;   2. outside bwrap, copy the selected files from staging to the project.

;;; Code:

(require 'sandbox-tools)
(require 'tabulated-list)
(require 'transient)

(defcustom sandbox-tools-review-exclude '(".git")
  "Directory names left out of the *sandbox diff* patch text.
Changes under them are still listed and can be applied."
  :type '(repeat string)
  :group 'sandbox-tools)

;;;; Helpers

(defmacro sandbox-tools--capture (&rest body)
  "Run BODY in a temporary buffer; return (VALUE . BUFFER-TEXT)."
  (declare (indent 0))
  `(with-temp-buffer (cons (progn ,@body) (buffer-string))))

(defun sandbox-tools--review (root staging &rest command)
  "Run COMMAND in a sandbox that shows project ROOT twice, read-only.
/real is the project as it is on the host; /merged is the project with
the sandbox's changes on top.  If STAGING is non-nil, it is writable at
/staging.  Output goes to the current buffer.  Return the exit status."
  (sandbox-tools--check-idle root)
  (let ((real (directory-file-name root)))
    (apply #'call-process "bwrap" nil t nil
           `(,@(sandbox-tools--base-args)
             "--ro-bind" ,real "/real"
             ,@(when staging `("--bind" ,staging "/staging"))
             ;; The last --overlay-src is the top layer.
             "--overlay-src" ,real
             "--overlay-src" ,(sandbox-tools--dir root "upper")
             "--ro-overlay" "/merged"
             "--" ,@command))))

(defun sandbox-tools--exit-ok-p (status &optional max)
  "Non-nil if `call-process' STATUS is an exit code no greater than MAX.
MAX defaults to 0.  A string STATUS (killed by a signal) is a failure."
  (and (integerp status) (<= status (or max 0))))

(defun sandbox-tools--status-desc (status)
  "Describe `call-process' STATUS for an error message."
  (if (integerp status)
      (format "exit %d" status)
    (format "aborted: %s" status)))

(defun sandbox-tools--step (label result)
  "Return the output of RESULT, a (STATUS . OUTPUT) pair.
If STATUS is a failure, show OUTPUT and signal an error naming LABEL."
  (pcase-let ((`(,status . ,output) result))
    (unless (sandbox-tools--exit-ok-p status)
      (sandbox-tools--display "*sandbox apply error*" output)
      (user-error "%s failed (%s); overlay retained"
                  label (sandbox-tools--status-desc status)))
    output))

(defun sandbox-tools--display (name text)
  "Show TEXT in a read-only buffer called NAME."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (special-mode))
    (pop-to-buffer (current-buffer))))

(defun sandbox-tools--project-buffers (root &optional modified)
  "Buffers visiting files under ROOT.
Return only modified buffers if MODIFIED is non-nil, else only unmodified."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (and buffer-file-name
            (file-in-directory-p buffer-file-name root)
            (if modified (buffer-modified-p) (not (buffer-modified-p))))))
   (buffer-list)))

(defun sandbox-tools--revert-project-buffers (root)
  "Revert unmodified buffers visiting files under ROOT and refresh VC."
  (dolist (buf (sandbox-tools--project-buffers root))
    (with-current-buffer buf
      (when (file-exists-p buffer-file-name)
        (ignore-errors (revert-buffer :ignore-auto :noconfirm)))))
  (ignore-errors (vc-refresh-state)))

;;;; Parsing rsync --itemize-changes output

(defun sandbox-tools--item (line)
  "Parse rsync -i LINE into (PATH . KIND), where KIND is `change' or `delete'.
Return nil if LINE names no path."
  (cond
   ((string-prefix-p "*deleting" line)
    (cons (string-trim (substring line 9)) 'delete))
   ((string-match "\\`[<>ch.*][fdLDS][^ ]* \\(.+\\)\\'" line)
    (cons (match-string 1 line) 'change))))

(defun sandbox-tools--hidden-p (path)
  "Non-nil if some component of relative PATH starts with a dot."
  (seq-some (lambda (part) (string-prefix-p "." part))
            (split-string path "/" t)))

;;;; Diff

(defun sandbox-tools--summary (root)
  "Return rsync's list of changes for ROOT, hidden paths last.
This shows changes `diff' cannot, such as empty files, modes and deletions."
  (pcase-let ((`(,status . ,output)
               (sandbox-tools--capture
                 (sandbox-tools--review root nil
                                        "rsync" "-ni" "-aH" "--delete" "--safe-links"
                                        "/merged/" "/real/"))))
    (unless (sandbox-tools--exit-ok-p status)
      (error "Failed to summarize sandbox changes: %s"
             (sandbox-tools--status-desc status)))
    (let* ((lines (seq-filter
                   (lambda (line)
                     ;; A leading "." means only attributes changed.
                     (let ((path (car (sandbox-tools--item line))))
                       (and path
                            (not (equal path "./"))
                            (not (string-prefix-p "." line)))))
                   (split-string output "\n" t "[ \t\r]+")))
           (hidden (lambda (line)
                     (sandbox-tools--hidden-p (car (sandbox-tools--item line))))))
      (append (seq-remove hidden lines) (seq-filter hidden lines)))))

(defun sandbox-tools--insert-diff (root)
  "Insert a unified diff of the sandbox changes for ROOT at point."
  (let ((status (apply #'sandbox-tools--review root nil
                       "diff" "-ruN"
                       (append (mapcan (lambda (dir) (list "-x" dir))
                                       sandbox-tools-review-exclude)
                               '("/real" "/merged")))))
    ;; diff exits with 1 when the files differ.
    (unless (sandbox-tools--exit-ok-p status 1)
      (error "Failed to generate sandbox diff: %s"
             (if (stringp status) status (string-trim (buffer-string))))))
  (replace-regexp-in-region "^\\(---\\|\\+\\+\\+\\) /real/" "\\1 a/" (point-min))
  (replace-regexp-in-region "^\\(---\\|\\+\\+\\+\\) /merged/" "\\1 b/" (point-min)))

(defvar-keymap sandbox-tools-diff-mode-map
  :doc "Extra bindings in *sandbox diff*, on top of `diff-mode'."
  "q" #'quit-window)

(defun sandbox-tools--diff (root)
  "Fill the *sandbox diff* buffer for ROOT.
Return non-nil if there are changes."
  (let ((summary (sandbox-tools--summary root)))
    (with-current-buffer (get-buffer-create "*sandbox diff*")
      (let ((inhibit-read-only t))
        (remove-overlays)
        (erase-buffer)
        (sandbox-tools--insert-diff root)
        (let ((changed (or summary (> (buffer-size) 0))))
          (goto-char (point-min))
          ;; diff-mode ignores lines starting with "#".
          (when summary
            (insert "# staged changes (rsync preview)\n"
                    (mapconcat (lambda (line) (concat "#   " line)) summary "\n")
                    "\n\n"))
          (unless changed
            (insert "No sandbox changes.\n"))
          (diff-mode)
          (use-local-map (make-composed-keymap sandbox-tools-diff-mode-map
                                               (current-local-map)))
          (setq header-line-format
                (substitute-command-keys
                 "\\<sandbox-tools-diff-mode-map>\\[quit-window] quit"))
          (setq buffer-read-only t)
          (goto-char (point-min))
          changed)))))

;;;###autoload
(defun sandbox-tools-diff ()
  "Show what the sandbox changed.  Return non-nil if anything changed."
  (interactive)
  (sandbox-tools--check-programs "bwrap" "rsync")
  (prog1 (sandbox-tools--diff (sandbox-tools-root))
    (pop-to-buffer "*sandbox diff*")))

;;;; Apply

(cl-defstruct (sandbox-tools-item (:constructor sandbox-tools-item--make))
  "One change that can be applied."
  path       ; relative path, ending in "/" for directories
  kind       ; `change' or `delete'
  hidden     ; non-nil for paths like .git/...
  selected)  ; non-nil if the user chose to apply it

(defun sandbox-tools--snapshot (root staging)
  "Phase 1: copy the merged project to STAGING.  Return (STATUS . OUTPUT)."
  (sandbox-tools--capture
    (sandbox-tools--review root staging
                           "rsync" "-aH" "--delete" "--safe-links"
                           "/merged/" "/staging/")))

(defun sandbox-tools--list-changes (root staging)
  "List what copying STAGING over ROOT would do, as rsync -i output.
Return (STATUS . OUTPUT)."
  (sandbox-tools--capture
    (call-process "rsync" nil t nil
                  "-n" "-aHi" "--delete" "--safe-links"
                  (file-name-as-directory staging)
                  (file-name-as-directory root))))

(defun sandbox-tools--build-plan (rsync-output)
  "Turn RSYNC-OUTPUT into a list of `sandbox-tools-item'.
Changed directories are left out, since copying their files creates them.
Hidden paths start out unselected."
  (let (items)
    (pcase-dolist (`(,path . ,kind)
                   (delq nil (mapcar #'sandbox-tools--item
                                     (split-string rsync-output "\n" t))))
      (unless (and (eq kind 'change) (string-suffix-p "/" path))
        (let ((hidden (sandbox-tools--hidden-p path)))
          (push (sandbox-tools-item--make :path path :kind kind
                                          :hidden hidden :selected (not hidden))
                items))))
    (nreverse items)))

(defun sandbox-tools--selected-paths (items kind)
  "Paths of the selected ITEMS of type KIND."
  (cl-loop for item in items
           when (and (sandbox-tools-item-selected item)
                     (eq (sandbox-tools-item-kind item) kind))
           collect (sandbox-tools-item-path item)))

(defun sandbox-tools--copy-files (staging root files)
  "Copy FILES, relative paths, from STAGING to ROOT.
Return (STATUS . OUTPUT)."
  (let ((list-file (make-temp-file "sandbox-tools-files")))
    (unwind-protect
        (progn
          (with-temp-file list-file
            (set-buffer-multibyte nil)
            (dolist (file files) (insert file "\0")))
          (sandbox-tools--capture
            (call-process "rsync" nil t nil
                          "-aHi" "--safe-links" "--from0"
                          (concat "--files-from=" list-file)
                          (file-name-as-directory staging)
                          (file-name-as-directory root))))
      (delete-file list-file))))

(defun sandbox-tools--delete-files (root files)
  "Delete FILES, relative paths that may be directories, under ROOT."
  (dolist (file files)
    (let ((abs (expand-file-name file root)))
      (cond ((file-directory-p abs) (delete-directory abs t))
            ((file-exists-p abs) (delete-file abs))))))

(defun sandbox-tools--modified-buffers-visiting (root files)
  "Modified buffers under ROOT visiting one of FILES (relative paths)."
  (let ((truenames (mapcar (lambda (file) (file-truename (expand-file-name file root)))
                           files)))
    (seq-filter (lambda (buf) (member (file-truename (buffer-file-name buf)) truenames))
                (sandbox-tools--project-buffers root t))))

(defun sandbox-tools--confirm-deletions (deletions)
  "Ask whether to delete DELETIONS on the host."
  (yes-or-no-p (format "Delete %d selected host file(s) (%s%s)? "
                       (length deletions)
                       (string-join (seq-take deletions 5) ", ")
                       (if (> (length deletions) 5) ", …" ""))))

(defun sandbox-tools--apply-plan (root staging items)
  "Phase 2: apply the selected ITEMS from STAGING to ROOT.
If every item was selected, discard the overlay afterwards."
  (let* ((changes   (sandbox-tools--selected-paths items 'change))
         (deletions (sandbox-tools--selected-paths items 'delete))
         (conflicts (sandbox-tools--modified-buffers-visiting
                     root (append changes deletions))))
    (when conflicts
      (user-error "Refusing to apply: modified buffer(s) visit selected \
files: %s (save or revert first)"
                  (mapconcat #'buffer-name conflicts ", ")))
    (when (and deletions (not (sandbox-tools--confirm-deletions deletions)))
      (user-error "Aborted; overlay retained"))
    (sandbox-tools--delete-files root deletions)
    (when changes
      (sandbox-tools--step "Apply (phase 2)"
                           (sandbox-tools--copy-files staging root changes)))
    (sandbox-tools--revert-project-buffers root)
    (if (cl-every #'sandbox-tools-item-selected items)
        (progn
          (sandbox-tools--discard root)
          (message "Applied all sandbox changes to %s" root))
      (message "Applied %d change(s) to %s; overlay retained"
               (+ (length changes) (length deletions)) root))))

;;;###autoload
(defun sandbox-tools-apply ()
  "Choose sandbox changes and copy them to the real project.
Refuses if a modified buffer visits a selected file.  If every change
is applied the overlay is discarded; otherwise it is kept."
  (interactive)
  (sandbox-tools--check-programs "bwrap" "rsync")
  (let ((root (sandbox-tools-root)))
    (unless (sandbox-tools--diff root)
      (user-error "No sandbox changes to apply"))
    (pop-to-buffer "*sandbox diff*")
    (let ((staging (sandbox-tools--dir root "staging")))
      (sandbox-tools--step "Snapshot (phase 1)"
                           (sandbox-tools--snapshot root staging))
      (let ((items (sandbox-tools--build-plan
                    (sandbox-tools--step "Listing changes"
                                         (sandbox-tools--list-changes root staging)))))
        (unless items
          (sandbox-tools--discard root)
          (user-error "Nothing to apply"))
        (sandbox-tools--select root staging items)))))

;;;; Selection buffer

(defface sandbox-tools-hidden '((t :inherit shadow :slant italic))
  "Face for hidden paths in *sandbox apply*."
  :group 'sandbox-tools)

(defvar-local sandbox-tools--sel-items nil "Items shown in *sandbox apply*.")
(defvar-local sandbox-tools--sel-root nil "Project root for *sandbox apply*.")
(defvar-local sandbox-tools--sel-staging nil "Staging directory for *sandbox apply*.")

(defconst sandbox-tools-select-keys
  '(("SPC"     sandbox-tools-select-toggle       "toggle")
    ("a"       sandbox-tools-select-all          "all")
    ("N"       sandbox-tools-select-none         "none")
    ("m"       sandbox-tools-select-mark-region   "mark region")
    ("u"       sandbox-tools-select-unmark-region "unmark region")
    ("t"       sandbox-tools-select-toggle-region "toggle region")
    ("C-c C-c" sandbox-tools-select-confirm      "apply")
    ("C-c C-k" sandbox-tools-select-abort        "abort")
    ("q"       sandbox-tools-select-abort        nil))
  "Keys of *sandbox apply* as (KEY COMMAND HINT); a nil HINT is not shown.")

(defvar sandbox-tools-select-mode-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key ,command ,_) sandbox-tools-select-keys)
      (keymap-set map key command))
    map)
  "Keymap for `sandbox-tools-select-mode'.")

(define-derived-mode sandbox-tools-select-mode tabulated-list-mode "SandboxSelect"
  "Choose which sandbox changes to apply."
  (setq tabulated-list-format [("" 3 nil) ("Kind" 8 nil) ("Path" 0 nil)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'sandbox-tools--sel-refresh nil t)
  (tabulated-list-init-header))

(defun sandbox-tools--sel-hints-row ()
  "A table row listing the main keys.  Its id is `hints'."
  (list 'hints
        (vector "" ""
                (mapconcat (pcase-lambda (`(,key ,_ ,hint))
                             (concat (propertize key 'face 'help-key-binding) " " hint))
                           (seq-filter #'caddr sandbox-tools-select-keys)
                           "  "))))

(defun sandbox-tools--sel-item-row (item)
  "Table row for ITEM.  Its id is ITEM itself."
  (let ((face (if (sandbox-tools-item-hidden item) 'sandbox-tools-hidden 'default)))
    (list item
          (vector (if (sandbox-tools-item-selected item) "[x]" "[ ]")
                  (propertize (symbol-name (sandbox-tools-item-kind item)) 'face face)
                  (propertize (sandbox-tools-item-path item) 'face face)))))

(defun sandbox-tools--sel-refresh ()
  "Redraw *sandbox apply*: key hints, visible rows, then hidden rows."
  (let ((visible (seq-remove #'sandbox-tools-item-hidden sandbox-tools--sel-items))
        (hidden  (seq-filter #'sandbox-tools-item-hidden sandbox-tools--sel-items)))
    (setq tabulated-list-entries
          (cons (sandbox-tools--sel-hints-row)
                (mapcar #'sandbox-tools--sel-item-row (append visible hidden)))))
  (tabulated-list-print t))

(defun sandbox-tools--sel-set (items value)
  "Set the `selected' flag of ITEMS to VALUE and redraw."
  (dolist (item items)
    (setf (sandbox-tools-item-selected item) value))
  (sandbox-tools--sel-refresh))

(defun sandbox-tools-select-toggle ()
  "Toggle the item at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless (sandbox-tools-item-p id)
      (user-error "Not on a change row"))
    (sandbox-tools--sel-set (list id) (not (sandbox-tools-item-selected id)))))

(defun sandbox-tools-select-all ()
  "Select every item."
  (interactive)
  (sandbox-tools--sel-set sandbox-tools--sel-items t))

(defun sandbox-tools-select-none ()
  "Deselect every item."
  (interactive)
  (sandbox-tools--sel-set sandbox-tools--sel-items nil))

(defun sandbox-tools--sel-region-items ()
  "Return items on rows intersecting the active region."
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((end (region-end))
        items)
    (save-excursion
      (goto-char (region-beginning))
      (beginning-of-line)
      (while (< (point) end)
        (let ((id (tabulated-list-get-id)))
          (when (sandbox-tools-item-p id)
            (push id items)))
        (forward-line 1)))
    (nreverse items)))

(defun sandbox-tools--sel-region-update (update)
  "Apply UPDATE to selected items on the active region's rows."
  (let ((items (sandbox-tools--sel-region-items)))
    (unless items
      (user-error "No change rows in region"))
    (dolist (item items)
      (setf (sandbox-tools-item-selected item)
            (funcall update (sandbox-tools-item-selected item))))
    (deactivate-mark)
    (sandbox-tools--sel-refresh)))

(defun sandbox-tools-select-mark-region ()
  "Select every change row in the active region."
  (interactive)
  (sandbox-tools--sel-region-update (lambda (_) t)))

(defun sandbox-tools-select-unmark-region ()
  "Deselect every change row in the active region."
  (interactive)
  (sandbox-tools--sel-region-update (lambda (_) nil)))

(defun sandbox-tools-select-toggle-region ()
  "Invert selection for every change row in the active region."
  (interactive)
  (sandbox-tools--sel-region-update #'not))

(defun sandbox-tools-select-confirm ()
  "Apply the selected items."
  (interactive)
  (let ((root sandbox-tools--sel-root)
        (staging sandbox-tools--sel-staging)
        (items sandbox-tools--sel-items))
    (unless (seq-some #'sandbox-tools-item-selected items)
      (user-error "Nothing selected"))
    (quit-window t)
    (sandbox-tools--apply-plan root staging items)))

(defun sandbox-tools-select-abort ()
  "Close the selection without applying anything."
  (interactive)
  (quit-window t)
  (message "Aborted; overlay retained"))

(defun sandbox-tools--select (root staging items)
  "Let the user choose which ITEMS to apply from STAGING to ROOT."
  (pop-to-buffer (get-buffer-create "*sandbox apply*"))
  (sandbox-tools-select-mode)
  (setq sandbox-tools--sel-root root
        sandbox-tools--sel-staging staging
        sandbox-tools--sel-items items)
  (sandbox-tools--sel-refresh))

;;;; Reset, interrupt, status, command

(defun sandbox-tools--delete-tree (dir)
  "Delete DIR recursively, even the unreadable dirs overlayfs creates."
  (when (file-exists-p dir)
    (call-process "chmod" nil nil nil "-R" "u+rwX" "--" dir)
    (delete-directory dir t)))

(defun sandbox-tools--discard (root)
  "Delete the overlay and staging data for ROOT.  Keeps /tmp and HOME."
  (sandbox-tools--check-idle root)
  (dolist (sub '("upper" "work" "staging"))
    (sandbox-tools--delete-tree (sandbox-tools--path root sub))))

;;;###autoload
(defun sandbox-tools-reset ()
  "Discard every change the sandbox made to the current project.
The sandbox's /tmp and HOME are kept."
  (interactive)
  (let ((root (sandbox-tools-root)))
    (sandbox-tools--check-idle root)
    (when (yes-or-no-p "Discard all sandbox changes? ")
      (sandbox-tools--discard root)
      (sandbox-tools--revert-project-buffers root)
      (message "Discarded sandbox changes for %s" root))))

;;;###autoload
(defun sandbox-tools-interrupt ()
  "Kill the sandbox commands running for the current project."
  (interactive)
  (let* ((root (sandbox-tools-root))
         (procs (sandbox-tools--processes root)))
    (mapc #'delete-process procs)
    (message "%d sandbox command(s) killed for %s"
             (length procs) (abbreviate-file-name root))))

;;;###autoload
(defun sandbox-tools-status ()
  "Say whether the current project has sandbox changes or a running command."
  (interactive)
  (let* ((root (sandbox-tools-root))
         (upper (sandbox-tools--path root "upper"))
         (changed (and (file-directory-p upper) (not (directory-empty-p upper)))))
    (message "Sandbox %s: %s%s"
             (abbreviate-file-name root)
             (if changed "staged data present" "empty")
             (if (or (gethash root sandbox-tools--running)
                     (gethash root sandbox-tools--queue))
                 ", command running" ""))))

;;;###autoload
(defun sandbox-tools-command (command)
  "Run shell COMMAND in the current project's sandbox and show its output."
  (interactive "sSandbox command: ")
  (sandbox-tools--run (lambda (result)
                        (sandbox-tools--display "*sandbox command*" result))
                      command))

;;;###autoload (autoload 'sandbox-tools-menu "sandbox-tools-review" nil t)
(transient-define-prefix sandbox-tools-menu ()
  "Sandbox commands."
  [["Review"
    ("d" "Show diff" sandbox-tools-diff)
    ("s" "Status"    sandbox-tools-status)]
   ["Run"
    ("!" "Command"   sandbox-tools-command)
    ("k" "Interrupt" sandbox-tools-interrupt)]
   ["Finalize"
    ("a" "Apply changes"   sandbox-tools-apply)
    ("r" "Discard changes" sandbox-tools-reset)]])

(provide 'sandbox-tools-review)
;;; sandbox-tools-review.el ends here
