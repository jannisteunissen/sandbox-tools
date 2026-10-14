# sandbox-tools

This package provides functionality to run emacs commands (such as
[gptel](https://github.com/karthink/gptel/) tool calls) in a
[bubblewrap](https://github.com/containers/bubblewrap) (`bwrap`) sandbox.
Changes to files are stored in an
[overlay](https://en.wikipedia.org/wiki/OverlayFS) and can afterwards be
merged into the host file system.

## Requirements

- `Emacs 29` or later.
- `bubblewrap` 0.9 or later, with overlayfs support
- `rsync` for copying from/to the sandbox

Optional:
- [gptel](https://github.com/karthink/gptel/) to use the included model tools
- `python3` for the `read_file` and `edit_file` LLM tools in the sandbox

## Installation

Place this directory somewhere and load it:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/sandbox-tools")
(require 'sandbox-tools)
```

If gptel is available, loading `sandbox-tools` will register `run_command`,
`read_file`, `write_file`, and `edit_file` with gptel under the `sandbox` category (see
below for details).

To bind the menu to a key use for example:

```elisp
(keymap-global-set "C-c s" #'sandbox-tools-menu)
```

## Functionality

### Directories

When a command or tool is called, the 'project' directory is found from the
`default-directory` of the current file, in this order:

1. The nearest parent folder with a `.git` entry
2. Emacs' current `project.el` root (if available)
3. `default-directory` itself is used

For each project, the module creates directories under
`sandbox-tools-scratch-dir` (default: `~/.emacs.d/sandbox/`) of the form
`<basename>-<hash>`. The main entries are:

- `upper/`: the sandbox's pending changes. Inside the sandbox, the project is
  mounted at `/workspace`. Files the sandbox creates or edits appear in
  `upper/` at the same relative path. Changes in `upper/` can be copied back
  through the `sandbox-tools-apply` method described below.
- `home/` and `tmp/`: sandbox `/home/sandbox` and `/tmp`. These persist across
  tool calls for that project.
- `staging/`: used internally for reviewing and applying changes.
- `work/`: private scratch space required for overlayfs functionality

`sandbox-tools-reset` removes `upper/`, `work/`, `staging/`, `home/` and `tmp/`.

### Sandbox permissions and environment

Each tool or command starts a new bubblewrap sandbox, with the following properties:

- Isolated namespaces (see `sandbox-tools--base-args`)
- A cleared minimal environment (see `sandbox-tools-env). Only variables in
  `sandbox-tools-preserve-env` are preserved from the host shell.
- A minimal `/dev` and `/proc`
- Read-only access to host paths in `sandbox-tools-ro-binds` (if defined)
- Network access is disabled, but `run_command` can request it. Approving shares
the host network, including access to localhost.

There are no writable host mounts, but they can be added via `sandbox-tools-cache-binds` as `(HOST-SRC . SANDBOX-DST)` pairs, for example:

```elisp
(setq sandbox-tools-cache-binds
      '(("~/.cache/some-tool" . "/home/sandbox/.cache/some-tool")))
```

## gptel tools

Tool calls do not ask for confirmation, except for network acces.

- **`run_command`** runs a Bash command from `/workspace`. Each call gets a
  fresh shell and environment. Commands are queued an execute sequentially per
  project. Output is capped; when it exceeds `sandbox-tools-max-output`, it is
  stored in a temporary file and only the start and end of the output are
  returned.
- **`read_file`** returns a text file's lines. Output is cut after
  `sandbox-tools-max-read-output` bytes. Requires Python 3 in the sandbox.
- **`write_file`** creates or overwrites a file in the project overlay.
- **`edit_file`** replaces a literal block in a file. Requires Python 3 in the
  sandbox.

## Reviewing, applying, and discarding changes

The following commands can be accessed through the `sandbox-tools-menu`:

- `sandbox-tools-diff` displays changes between sandbox and host
- `sandbox-tools-apply` to copy the selected items to the host project
- `sandbox-tools-reset` to discard the changes in the sandbox
- `sandbox-tools-status` reports whether a command is running and whether there are changes
- `sandbox-tools-interrupt` kills running command processes for the current project
- `sandbox-tools-command` run a custom command in the sandbox

## Configuration

Options are in the `sandbox-tools` customization group.
