;;; esync.el --- Emacs file sync -- async multi-destination deployment -*- lexical-binding: t -*-

;; Copyright (C) 2025 Claude

;; Author: Claude <claude@anthropic.com>
;; Created: 2025
;; Version: 0.2.0
;; Keywords: files comm deploy
;; URL: https://github.com/your-name/esync
;; Package-Requires: ((emacs "28.1") (async "1.9"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `esync-mode' is a minor mode that syncs the current file to one or more
;; destination paths.  All transfers are asynchronous (async.el subprocess or
;; make-thread) so Emacs is never blocked.  TRAMP remote paths are supported.
;;
;; ## Setup
;;
;;   (global-esync-mode)
;;
;; ## .dir-locals.el - single destination
;;
;;   ((nil . ((esync-dest-dir . "/scp:server:/srv/proj")
;;            (esync-ignore-patterns . ("/cache")))))
;;
;; ## .dir-locals.el - multiple destinations
;;
;;   ((nil . ((esync-dest-dir . ("/sshx:server1:/srv/proj"
;;                               "/rpc:server2:/srv/proj"))
;;            (esync-ignore-patterns . ("/cache")))))
;;
;; ## Commands
;;
;;   M-x esync-upload    -- manually push current file/dir to all destinations
;;   M-x esync-download  -- pull current file/dir from a chosen destination
;;                          (prompts when multiple destinations are configured)
;;                          and reverts the buffer automatically.
;;
;; Automatic upload on save is provided by `esync-mode' / `global-esync-mode'.
;;
;; ## TRAMP + async notes
;;
;; - Plain methods (scp, ssh, sshx, …) run in an async child process started
;;   with `-q' so ~/.authinfo / ~/.netrc is readable.
;; - Methods that need a live parent-process connection (e.g. /rpc: from
;;   tramp-rpc) run in a `make-thread' instead; add method names to
;;   `esync-tramp-thread-methods' as needed.
;; - Set `esync-async-init-file' to load extra setup in child processes.
;;
;; See the TRAMP User Manual for path syntax:
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Configuration.html

;;; Code:

(require 'cl-lib)
(require 'async)

;;;; Customization

(defgroup esync nil
  "Async multi-destination file sync for Emacs."
  :group 'files)

(defcustom esync-remove-mode-line-unless-availabled t
  "Hide the mode-line lighter when `esync' is not active."
  :type 'boolean
  :group 'esync)

(defcustom esync-default-lighter " Esync"
  "Minor mode lighter shown in the mode-line."
  :type 'string
  :group 'esync)

(defcustom esync-verbose t
  "When non-nil, report async copy results in the echo area."
  :type 'boolean
  :group 'esync)

(defcustom esync-async-init-file nil
  "Path to an Elisp file loaded in every async child process.
Useful for setting up TRAMP auth-sources or load-path.
When nil the child starts with `-q' and no extra init file."
  :type '(choice (const :tag "None" nil) file)
  :group 'esync)

;;;; Constants / internal vars

(defconst esync-root-markers '(".git" ".project")
  "Marker files/dirs used to detect the project root.")

(defvar-local esync-lighter esync-default-lighter)

;;;; Directory-local variables

;;;###autoload
(progn
  (defvar-local esync-dest-dir nil
    "Destination(s) for deployment.
A string means a single destination directory; a list of strings means
multiple destinations.  Each path may be local or a TRAMP remote path.")
  (put 'esync-dest-dir 'safe-local-variable
       (lambda (obj)
         (or (stringp obj)
             (and (listp obj) (cl-every #'stringp obj))))))

;;;###autoload
(progn
  (defvar-local esync-ignore-patterns '("/\\.dir-locals\\.el\\'" "/\\.git/")
    "Skip deployment when `buffer-file-name' matches any of these regexps.")
  (put 'esync-ignore-patterns 'safe-local-variable
       (lambda (obj)
         (and (listp obj) (cl-every #'stringp obj)))))

;;;###autoload
(progn
  (defvar-local esync-project-root-marker nil
    "Override marker file used to locate the project root.
When nil, `esync--detect-project-root' walks up the directory tree
looking for any entry in `esync-root-markers' (.git or .project).")
  (put 'esync-project-root-marker 'safe-local-variable
       (lambda (obj) (or (null obj) (stringp obj)))))

(defvar esync-base-dir nil
  "Override base directory for path-stripping.  Normally auto-detected.")

;;;; Internal helpers

(defun esync--dest-dirs ()
  "Return `esync-dest-dir' normalised to a list of strings."
  (cond
   ((null esync-dest-dir) nil)
   ((stringp esync-dest-dir) (list esync-dest-dir))
   ((listp esync-dest-dir) esync-dest-dir)
   (t (error "esync-dest-dir has unsupported type: %S"
             esync-dest-dir))))

(defun esync--not-matches-ignore-patterns (filename)
  "Return t when FILENAME does not match any ignore pattern."
  (cl-loop for pat in esync-ignore-patterns
           never (string-match-p pat filename)))

(defun esync--available ()
  "Return non-nil when the current buffer should be deployed."
  (and buffer-file-name
       (esync--dest-dirs)
       (esync--not-matches-ignore-patterns buffer-file-name)))

(defun esync--update-lighter (availablep)
  "Show or hide the mode-line lighter based on AVAILABLEP."
  (setq esync-lighter
        (if (or availablep
                (not esync-remove-mode-line-unless-availabled))
            esync-default-lighter
          nil)))

(defun esync--detect-project-root ()
  "Return the project root by walking up from the current buffer's file.
Checks each directory for the presence of any entry in `esync-root-markers'
(.git or .project by default).  Falls back to the buffer's directory if no
marker is found.  Set `esync-project-root-marker' to force a specific marker."
  (let* ((markers (if esync-project-root-marker
                      (list esync-project-root-marker)
                    esync-root-markers))
         (found (cl-some (lambda (marker)
                           (locate-dominating-file buffer-file-name marker))
                         markers)))
    (file-truename (or found (file-name-directory buffer-file-name)))))

(defun esync--base-dir ()
  "Return the base directory used when computing destination paths."
  (cond
   ((null esync-base-dir) (esync--detect-project-root))
   ((stringp esync-base-dir) esync-base-dir)
   ((fboundp esync-base-dir) (funcall esync-base-dir))
   (t (error "esync-base-dir `%s' is invalid"
             esync-base-dir))))

(defun esync--replace-path (src-file-path dest-dir)
  "Compute the destination path for SRC-FILE-PATH inside DEST-DIR.
DEST-DIR may be any path that Emacs can write to, including TRAMP
paths such as /scp:host:/path or /rcp:host:/path."
  ;; Strip file-name handlers only for the string manipulation so that
  ;; TRAMP paths in src/base-dir are treated as plain strings.
  ;; expand-file-name must run with handlers intact so that TRAMP can
  ;; interpret dest-dir correctly.
  (let ((relative (let (file-name-handler-alist)
                    (string-replace (esync--base-dir) "" src-file-path))))
    (expand-file-name relative dest-dir)))

;;;; Async copy

(defcustom esync-tramp-thread-methods '("rpc")
  "TRAMP methods that require a live parent-process connection.
Paths using these methods are copied via `make-thread' in the parent
Emacs process rather than in an async child process, because they rely
on state (connections, loaded packages, running servers) that a fresh
child process cannot replicate.  `tramp-rpc' (/rpc:) is the canonical
example: it needs its SSH ControlMaster and the full tramp-rpc package
already loaded.  Plain methods like ssh/scp/scpx work fine in a child
and are NOT listed here."
  :type '(repeat string)
  :group 'esync)

(defun esync--tramp-method (path)
  "Return the TRAMP method string for PATH, or nil if PATH is not a TRAMP path."
  (and (file-remote-p path)
       (require 'tramp)
       (tramp-file-name-method (tramp-dissect-file-name path))))

(defun esync--needs-thread-p (to-path)
  "Return non-nil if TO-PATH must be copied via thread rather than subprocess.
This is true when the TRAMP method for TO-PATH is listed in
`esync-tramp-thread-methods', meaning it requires live parent-process
state (open connections, loaded packages) that a child cannot replicate."
  (let ((method (esync--tramp-method to-path)))
    (and method (member method esync-tramp-thread-methods))))

(defun esync--do-copy (from-path to-path verbose)
  "Perform the actual copy FROM-PATH → TO-PATH and report result.
Intended to run either in a thread or an async child process.
VERBOSE controls whether a message is emitted on completion."
  (let ((to-dir (file-name-directory to-path)))
    (unless (file-exists-p to-dir)
      (make-directory to-dir t))
    (condition-case err
        (progn
          (copy-file from-path to-path t)
          (when verbose
            (message "esync: OK %s -> %s" from-path to-path)))
      (error
       (message "esync: FAILED %s -> %s\n  %s"
                from-path to-path (error-message-string err))))))

(defun esync--copy-via-thread (from-path to-path verbose)
  "Copy FROM-PATH → TO-PATH in a background thread inside the parent process.
Used for TRAMP methods in `esync-tramp-thread-methods' (e.g. /rpc:)
that depend on live connections and packages already loaded in the parent.
The caller (`esync--async-copy') is responsible for pre-warming the
TRAMP connection on the main thread before calling this."
  (make-thread
   (lambda () (esync--do-copy from-path to-path verbose))
   (format "esync %s" (file-name-nondirectory from-path))))

(defun esync--copy-via-subprocess (from-path to-path verbose)
  "Copy FROM-PATH → TO-PATH in an async child Emacs process via `async-start'.
Used for TRAMP methods (scp, ssh, scpx, …) that can authenticate
independently in a fresh process using ~/.authinfo / ~/.netrc.
`async-quiet-switch' is set to \"-q\" so auth-source files are readable.
Set `esync-async-init-file' for any additional child-process setup."
  (let ((async-quiet-switch "-q")
        (async-child-init esync-async-init-file))
    (async-start
     `(lambda ()
        (require 'tramp)
        (let ((to-dir (file-name-directory ,to-path)))
          (unless (file-exists-p to-dir)
            (make-directory to-dir t))
          (condition-case err
              (progn
                (copy-file ,from-path ,to-path t)
                (list :ok t :from ,from-path :to ,to-path))
            (error
             (list :ok nil
                   :from ,from-path
                   :to   ,to-path
                   :error (error-message-string err))))))
     (lambda (result)
       (when verbose
         (if (plist-get result :ok)
             (message "esync: OK %s -> %s"
                      (plist-get result :from)
                      (plist-get result :to))
           (message "esync: FAILED %s -> %s\n  %s"
                    (plist-get result :from)
                    (plist-get result :to)
                    (plist-get result :error))))))))

(defun esync--ensure-tramp-connection (path)
  "Ensure the TRAMP connection for PATH is open, in the main thread.
This must be called before handing off any TRAMP copy to a subprocess
or background thread.  It guarantees that:
- Interactive auth prompts (password, host-key confirmation) are
  presented to the user in the normal minibuffer, where they can
  respond or press \\[keyboard-quit] to cancel cleanly.
- Subsequent async operations can reuse the already-open connection
  without needing to authenticate again.

For non-TRAMP (local) paths this is a no-op."
  (when-let ((remote-root (file-remote-p path)))
    ;; file-exists-p is the canonical lightweight probe: it opens the
    ;; connection if needed, authenticates, and returns quickly.
    ;; Any error or C-g here surfaces naturally to the user.
    (file-exists-p remote-root)))

(defun esync--async-copy (from-path to-path)
  "Asynchronously copy FROM-PATH to TO-PATH, choosing the right strategy.

For any remote path the TRAMP connection is first established on the
main thread (via `esync--ensure-tramp-connection') so that interactive
auth prompts work and C-g cancels cleanly.  Once connected:

- Methods in `esync-tramp-thread-methods' (e.g. /rpc:) use
  `make-thread' to reuse the live parent-process connection.
- All other paths use `async-start' in a fresh child process."
  (let ((verbose esync-verbose)
        ;; Determine strategy before pre-warming, so we only probe the
        ;; relevant end of the transfer (destination for upload, which
        ;; is always the remote side).
        (use-thread (esync--needs-thread-p to-path)))
    ;; Pre-warm: open connection interactively if not already open.
    ;; This is a no-op when the connection is already cached.
    (esync--ensure-tramp-connection to-path)
    (if use-thread
        (esync--copy-via-thread from-path to-path verbose)
      (esync--copy-via-subprocess from-path to-path verbose))))

(defun esync--copy-file ()
  "Fire off async copies of the current buffer's file to all destinations."
  (let* ((from-path buffer-file-name)
         (base-dir  (esync--base-dir)))
    (dolist (dest-dir (esync--dest-dirs))
      (esync--async-copy
       from-path
       (esync--replace-path from-path dest-dir)))))

;;;; Hook

(defun esync--hook-after-save ()
  "Hook function added to `after-save-hook' by `esync-mode'."
  (if (esync--available)
      (progn
        (esync--update-lighter t)
        (esync--copy-file))
    (esync--update-lighter nil)))

;;;; Interactive upload / download commands

(defun esync--current-path ()
  "Return the local path to act on: buffer file, or current directory."
  (cond
   ((and buffer-file-name (file-exists-p buffer-file-name))
    (file-truename buffer-file-name))
   ((and default-directory (file-exists-p default-directory))
    (file-truename default-directory))))

(defun esync--remote-path-for (local-path dest-dir)
  "Compute the remote path in DEST-DIR that corresponds to LOCAL-PATH."
  (esync--replace-path local-path dest-dir))

(defun esync--local-path-for (remote-path dest-dir)
  "Compute the local path that corresponds to REMOTE-PATH in DEST-DIR.
This is the inverse of `esync--remote-path-for'."
  (let* ((base-dir  (esync--base-dir))
         ;; Strip dest-dir prefix from remote-path to get the relative part.
         ;; We must inhibit file-name handlers so TRAMP paths are plain strings.
         (relative  (let (file-name-handler-alist)
                      (string-replace (file-name-as-directory dest-dir)
                                      ""
                                      (expand-file-name remote-path)))))
    (expand-file-name relative base-dir)))

;;;###autoload
(defun esync-upload ()
  "Upload the current file (or directory) to all configured destinations.
Respects `esync-ignore-patterns'.  Uses the same async strategy as the
automatic on-save upload (thread for methods in `esync-tramp-thread-methods',
subprocess for everything else)."
  (interactive)
  (let ((dest-dirs (esync--dest-dirs))
        (local-path (esync--current-path)))
    (cond
     ((null dest-dirs)
      (user-error "esync: no destinations configured (set esync-dest-dir in .dir-locals.el)"))
     ((null local-path)
      (user-error "esync: no file or directory to upload"))
     ((not (esync--not-matches-ignore-patterns local-path))
      (user-error "esync: %s is excluded by esync-ignore-patterns" local-path))
     (t
      (dolist (dest-dir dest-dirs)
        (let ((remote-path (esync--remote-path-for local-path dest-dir)))
          (when esync-verbose
            (message "esync: uploading %s -> %s" local-path remote-path))
          (esync--async-copy local-path remote-path)))))))

;;;###autoload
(defun esync-download ()
  "Download the current file (or directory) from each configured destination.
When multiple destinations are configured, prompts you to choose one.
After a successful download the current buffer is reverted."
  (interactive)
  (let ((dest-dirs (esync--dest-dirs))
        (local-path (esync--current-path)))
    (cond
     ((null dest-dirs)
      (user-error "esync: no destinations configured (set esync-dest-dir in .dir-locals.el)"))
     ((null local-path)
      (user-error "esync: no file or directory to download"))
     (t
      (let* ((dest-dir
              (if (cdr dest-dirs)
                  ;; More than one destination: ask which remote to pull from.
                  (completing-read "esync download from: " dest-dirs nil t)
                (car dest-dirs)))
             (remote-path (esync--remote-path-for local-path dest-dir)))
        (when esync-verbose
          (message "esync: downloading %s -> %s" remote-path local-path))
        ;; Download runs async; revert buffer in the callback / thread finish.
        (if (esync--needs-thread-p remote-path)
            (make-thread
             (lambda ()
               (esync--do-copy remote-path local-path esync-verbose)
               ;; Revert must happen on the main thread.
               (run-at-time 0 nil #'esync--revert-buffer local-path))
             (format "esync-dl %s" (file-name-nondirectory local-path)))
          (let ((async-quiet-switch "-q")
                (async-child-init esync-async-init-file)
                (verbose esync-verbose))
            (async-start
             `(lambda ()
                (require 'tramp)
                (let ((local-dir (file-name-directory ,local-path)))
                  (unless (file-exists-p local-dir)
                    (make-directory local-dir t))
                  (condition-case err
                      (progn
                        (copy-file ,remote-path ,local-path t)
                        (list :ok t :from ,remote-path :to ,local-path))
                    (error
                     (list :ok nil
                           :from ,remote-path
                           :to   ,local-path
                           :error (error-message-string err))))))
             (lambda (result)
               (if (plist-get result :ok)
                   (progn
                     (when verbose
                       (message "esync: OK %s -> %s"
                                (plist-get result :from)
                                (plist-get result :to)))
                     (esync--revert-buffer (plist-get result :to)))
                 (message "esync: FAILED %s -> %s\n  %s"
                          (plist-get result :from)
                          (plist-get result :to)
                          (plist-get result :error))))))))))))

(defun esync--revert-buffer (local-path)
  "Revert a buffer visiting LOCAL-PATH if one exists."
  (let ((buf (find-buffer-visiting local-path)))
    (when buf
      (with-current-buffer buf
        (revert-buffer t t t)))))

;;;; Minor mode

;;;###autoload
(defun turn-on-esync ()
  "Turn on `esync-mode' in the current buffer."
  (esync--update-lighter (esync--available))
  (esync-mode 1))

;;;###autoload
(define-minor-mode esync-mode
  "Minor mode for automatic deployment/synchronisation on save."
  :group 'esync
  :lighter esync-lighter
  (if esync-mode
      (add-hook 'after-save-hook #'esync--hook-after-save nil t)
    (remove-hook 'after-save-hook #'esync--hook-after-save t)))

;;;###autoload
(define-globalized-minor-mode global-esync-mode esync-mode
  turn-on-esync
  :group 'esync
  :require 'esync)

(provide 'esync)
;;; esync.el ends here
