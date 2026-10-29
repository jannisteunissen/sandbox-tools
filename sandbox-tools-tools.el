;;; sandbox-tools-tools.el --- gptel tools for sandbox-tools -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna

;;; Commentary:

;; Register the run_command, read_file, write_file and edit_file tools with
;; gptel. Loading this file registers the tools.

;;; Code:

(require 'gptel)
(require 'sandbox-tools)

;;;; Argument helpers

(defun sandbox-tools--true-p (value)
  "Non-nil if tool argument VALUE is an explicit true value.
True values are t, :json-true, a non-zero number, or one of the strings
\"true\", \"t\", \"yes\" and \"1\".  Everything else is false."
  (pcase value
    ('t t)
    (:json-true t)
    ((pred stringp) (and (member (downcase (string-trim value))
                                 '("true" "t" "yes" "1"))
                         t))
    ((pred numberp) (/= value 0))
    (_ nil)))

(defun sandbox-tools--string-arg (name value &optional allow-empty)
  "Return tool argument NAME's VALUE, checking that it is a string.
If ALLOW-EMPTY is non-nil, \"\" is accepted and nil or :null become \"\"."
  (cond ((and (stringp value) (or allow-empty (not (string-empty-p value))))
         value)
        ((and allow-empty (memq value '(nil :null)))
         "")
        (t (user-error "Invalid argument `%s': expected a %sstring, got %S"
                       name (if allow-empty "" "non-empty ") value))))

(defmacro sandbox-tools--with-cb (callback &rest body)
  "Run BODY, reporting any error or quit to CALLBACK.
An async gptel tool must always call its callback, or the request hangs."
  (declare (indent 1) (debug (form body)))
  (let ((cb (make-symbol "cb")))
    `(let ((,cb ,callback))
       (condition-case err
           (progn ,@body)
         (quit  (funcall ,cb "Cancelled by user."))
         (error (funcall ,cb (format "Tool error: %s"
                                     (error-message-string err))))))))

;;;; run_command

(defcustom sandbox-tools-confirm-network t
  "Whether to ask before running a command with network access.
If nil, network commands run without confirmation."
  :type 'boolean
  :group 'sandbox-tools)

(defvar sandbox-tools--network-always-allowed nil
  "Non-nil if the user chose to allow network access for this session.")

(defun sandbox-tools-reset-network-approval ()
  "Ask again before running network commands."
  (interactive)
  (setq sandbox-tools--network-always-allowed nil)
  (message "sandbox-tools: network commands will require approval again"))

(defun sandbox-tools--confirm-network (command)
  "Ask the user whether COMMAND may run with network access.
Choosing \"always\" skips the prompt for the rest of the session."
  (or (not sandbox-tools-confirm-network)
      sandbox-tools--network-always-allowed
      (pcase (car (read-multiple-choice
                   (format "Sandbox [NETWORK — host net & localhost!]: run `%s'? "
                           (truncate-string-to-width
                            (replace-regexp-in-string "\n" "⏎" command)
                            120 nil nil "…"))
                   '((?y "yes" "Run this command with network access")
                     (?n "no" "Deny network access for this command")
                     (?a "always" "Allow network access without asking \
for the rest of this session"))))
        (?y t)
        (?a (setq sandbox-tools--network-always-allowed t))
        (_ nil))))

(gptel-make-tool
 :name "run_command" :category "sandbox" :async t :include t :confirm nil
 :description "Run a bash command in a sandbox (cwd: /workspace, the project root).

- Edit files with `write_file'/`edit_file'.
- cwd resets each call to /workspace (use `cd dir && …` for other paths);
- /tmp persists.
- Minimal env; resets each call; no network unless `network' is true.
- stderr is merged into stdout; long output is truncated (head+tail)
- Batch independent calls in one response; they run in order."
 :args '((:name "command" :type string
                :description "Bash command line.")
         (:name "network" :type boolean :optional t
                :description "true only if internet/localhost access is needed \
(requires user approval)."))
 :function
 (lambda (cb command &optional network &rest _)
   (sandbox-tools--with-cb cb
     (let ((command (sandbox-tools--string-arg "command" command))
           (network (sandbox-tools--true-p network)))
       (if (and network (not (sandbox-tools--confirm-network command)))
           (funcall cb "User denied network execution of this command.")
         (sandbox-tools--run cb command network))))))

;;;; write_file

(defconst sandbox-tools--write-script "\
f=%s
old=0
[ -f \"$f\" ] && old=$(wc -c < \"$f\")
mkdir -p \"$(dirname \"$f\")\" || exit 1
cat > \"$f\" || exit 1
new=$(wc -c < \"$f\")
echo \"wrote $f: $new bytes (was $old bytes)\"
if [ \"$new\" -eq 0 ]; then
  echo 'WARNING: file now EMPTY — content likely truncated.'
fi"
  "Shell script for `write_file'; %s is the quoted path, content is stdin.")

(gptel-make-tool
 :name "write_file" :category "sandbox" :async t :include t :confirm nil
 :description "Create or OVERWRITE a file with the full `content'.
Use for new files or full rewrites of small files; prefer edit_file for targeted edits.
Returns byte count"
 :args '((:name "path" :type string
                :description "Path relative to project root")
         (:name "content" :type string
                :description "Complete file contents, not a patch"))
 :function
 (lambda (cb path content &rest _)
   (sandbox-tools--with-cb cb
     (let ((path (sandbox-tools--string-arg "path" path))
           (content (sandbox-tools--string-arg "content" content t)))
       (sandbox-tools--check-path path)
       (sandbox-tools--run cb
                           (format sandbox-tools--write-script
                                   (shell-quote-argument path))
                           nil content)))))

;;;; edit_file

(defconst sandbox-tools--dir
  (file-name-directory
   (or load-file-name
       (bound-and-true-p byte-compile-current-file)
       (locate-library "sandbox-tools-tools")
       buffer-file-name
       (error "sandbox-tools: cannot locate library directory")))
  "Directory containing sandbox-tools-tools.el and its helper scripts.")

(defun sandbox-tools--script-source (file)
  "Return the contents of FILE, found next to this library."
  (let ((path (expand-file-name file sandbox-tools--dir)))
    (unless (file-readable-p path)
      (error "sandbox-tools: missing helper script %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun sandbox-tools--python-command (script &rest args)
  "Shell command running Python SCRIPT (source text) with ARGS."
  (format "command -v python3 >/dev/null 2>&1 || \
{ echo 'FAILED: python3 is not available in the sandbox'; exit 127; }
exec python3 -c %s %s"
          (shell-quote-argument script)
          (mapconcat #'shell-quote-argument args " ")))

(defconst sandbox-tools--edit-script
  (sandbox-tools--script-source "edit_file.py")
  "Source of edit_file.py, which runs inside the sandbox for `edit_file'.")

(defun sandbox-tools--edit-input (old new)
  "Standard input for edit_file.py: byte counts of OLD and NEW, then both."
  (let ((bytes (lambda (s) (length (encode-coding-string s 'utf-8-unix)))))
    (format "%d %d\n%s%s" (funcall bytes old) (funcall bytes new) old new)))

(gptel-make-tool
 :name "edit_file" :category "sandbox" :async t :include t :confirm nil
 :description "Replace a block of text in an existing file under /workspace.
Prefer this over write_file for targeted edits.

- `old': literal text copied verbatim from the file (read it first), with
  enough surrounding lines to be unique.  Not a diff: no +/-/@@ markers.
- `new': replacement text (\"\" deletes).
- If no exact match, a whitespace-tolerant match is tried and re-indented.
- On failure, nothing changes; you get the reason and closest lines. Retry.
- On success, returns the applied diff."
 :args '((:name "path" :type string
                :description "Path relative to project root (no leading / or ..).")
         (:name "old" :type string
                :description "Verbatim text to replace.")
         (:name "new" :type string
                :description "Replacement text; \"\" deletes.")
         (:name "replace_all" :type boolean :optional t
                :description "true to replace every occurrence; omit otherwise."))
 :function
 (lambda (cb path old new &optional replace_all &rest _)
   (sandbox-tools--with-cb cb
     (let ((path (sandbox-tools--string-arg "path" path))
           (old  (sandbox-tools--string-arg "old" old))
           (new  (sandbox-tools--string-arg "new" new t))
           (mode (if (sandbox-tools--true-p replace_all) "all" "one")))
       (sandbox-tools--check-path path)
       (sandbox-tools--run
        cb
        (sandbox-tools--python-command sandbox-tools--edit-script path mode)
        nil
        (sandbox-tools--edit-input old new))))))

;;;; read_file

(defconst sandbox-tools--read-script
  (sandbox-tools--script-source "read_file.py")
  "Source of read_file.py, which runs inside the sandbox for `read_file'.")

(defun sandbox-tools--int-arg (name value default)
  "Return tool argument NAME's VALUE as a non-negative integer.
Return DEFAULT if VALUE is absent (nil, :null or \"\")."
  (let ((n (cond ((memq value '(nil :null)) default)
                 ((and (stringp value) (string-empty-p (string-trim value)))
                  default)
                 ((and (stringp value)
                       (string-match-p "\\`[ \t]*[0-9]+[ \t]*\\'" value))
                  (string-to-number value))
                 ((numberp value) (truncate value))
                 (t value))))
    (unless (natnump n)
      (user-error "Invalid argument `%s': expected a non-negative integer, got %S"
                  name value))
    n))

(gptel-make-tool
 :name "read_file" :category "sandbox" :async t :include t :confirm nil
 :description "Read a text file in the sandbox (/workspace), with a much
larger output limit than run_command.

  * Output starts with a header \"PATH: lines A-B of N\", then the raw lines
    (no line-number prefixes — copy text from it verbatim for edit_file).
  * Long files are cut at a line boundary; a trailing note then gives the
    `offset' to continue from.  Use `offset'/`limit' to read a range.
  * For searching, prefer run_command with grep -n.
  * When possible issue multiple independent reads in one response."
 :args '((:name "path" :type string
                :description "Relative path from project root (no leading / or ..).")
         (:name "offset" :type integer :optional t
                :description "1-based line number to start at (default 1).")
         (:name "limit" :type integer :optional t
                :description "Maximum number of lines to return (default: \
as many as fit in the output limit)."))
 :function
 (lambda (cb path &optional offset limit &rest _)
   (sandbox-tools--with-cb cb
     (let ((path   (sandbox-tools--string-arg "path" path))
           (offset (sandbox-tools--int-arg "offset" offset 1))
           (limit  (sandbox-tools--int-arg "limit" limit 0)))
       (sandbox-tools--check-path path)
       ;; read_file.py caps the content itself, at a line boundary.  Raise
       ;; the generic head+tail cap so it leaves room for header and notes.
       ;; Passed explicitly: the wrapper is built when the job is dequeued.
       (sandbox-tools--run
        cb
        (sandbox-tools--python-command
         sandbox-tools--read-script path
         (number-to-string offset) (number-to-string limit)
         (number-to-string sandbox-tools-max-read-output))
        nil nil
        (max sandbox-tools-max-output
             (+ sandbox-tools-max-read-output 4096 (string-bytes path))))))))

(provide 'sandbox-tools-tools)
;;; sandbox-tools-tools.el ends here
