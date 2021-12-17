;;; julia-snail.el --- Julia Snail -*- lexical-binding: t -*-


;; URL: https://github.com/gcv/julia-snail
;; Package-Requires: ((emacs "26.2") (dash "2.16.0") (julia-mode "0.3") (s "1.12.0") (spinner "1.7.3") (vterm "0.0.1"))
;; Version: 1.1.4
;; Created: 2019-10-27

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interactive development environment for Julia
;; (https://julialang.org/), similar to SLIME for Common Lisp and CIDER for
;; Clojure. Refer to the README.md file for documentation.

;;; Code:


;;; --- requirements

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'pulse)
(require 'rx)
(require 's)
(require 'spinner)
(require 'subr-x)
(require 'thingatpt)
(require 'vterm)
(require 'xref)


;;; --- customization

(defgroup julia-snail nil
  "Customization options for Julia Snail mode."
  :group 'external)

(defcustom julia-snail-executable "julia"
  "Julia executable to run as a Snail server."
  :tag "Julia executable"
  :group 'julia-snail
  :safe 'stringp
  :type 'string)
(make-variable-buffer-local 'julia-snail-executable)

(defcustom julia-snail-extra-args nil
  "Extra arguments to pass to the Julia binary, e.g. '--sysimage /path/to/image'."
  :tag "Extra arguments (string or list of strings)"
  :group 'julia-snail
  :safe (lambda (obj) (or (null obj) (stringp obj) (listp obj)))
  :type '(choice (const :tag "None" nil)
                 (string :tag "Single string")
                 (repeat :tag "List of strings" string)))
(make-variable-buffer-local 'julia-snail-extra-args)

(defcustom julia-snail-port 10011
  "Default Snail server port for Emacs to connect to."
  :tag "Snail server port (local)"
  :group 'julia-snail
  :safe 'integerp
  :type 'integer)
(make-variable-buffer-local 'julia-snail-port)

(defcustom julia-snail-remote-port nil
  "Default Snail server port when using a remote REPL. Do not set UNLESS using a remote REPL!"
  :tag "Snail server port (remote); do not set unless using remote REPL"
  :group 'julia-snail
  :safe (lambda (obj) (or (null obj) (integerp obj)))
  :type '(choice (const :tag "Same as local" nil)
                 (integer)))
(make-variable-buffer-local 'julia-snail-remote-port)

(defcustom julia-snail-repl-buffer "*julia*"
  "Default buffer to use for Julia REPL interaction."
  :tag "Julia REPL buffer"
  :group 'julia-snail
  :safe 'stringp
  :type 'string)
(make-variable-buffer-local 'julia-snail-repl-buffer)

(defcustom julia-snail-show-error-window t
  "When t: show compilation errors in separate window. When nil: display errors in the minibuffer."
  :tag "Show compilation errors in separate window"
  :group 'julia-snail
  :type 'boolean)

(defcustom julia-snail-async-timeout 20000
  "When performing asynchronous Snail operations, wait this many milliseconds before timing out."
  :tag "Timeout for asynchronous Snail operations"
  :group 'julia-snail
  :safe 'integerp
  :type 'integer)

(defcustom julia-snail-multimedia-enable nil
  "When t: enable Emacs integration with the Julia multimedia system."
  :tag "Enable Julia multimedia integration"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)
(make-variable-buffer-local 'julia-snail-multimedia-enable)

(defcustom julia-snail-multimedia-buffer-autoswitch nil
  "If true, when an image is displayed inside Emacs, the
multimedia buffer gets the focus (e.g., for zooming and panning).
If nil, the image window is displayed but focus remains on the
REPL buffer."
  :tag "Automatically switch to multimedia (plot) content buffer"
  :group 'julia-snail
  :type 'boolean)

(defcustom julia-snail-multimedia-buffer-style :single-reuse
  "Controls multimedia buffer behavior. When
:single-reuse (default), reuse the same buffer to show every
image; this erases previous images. When :single-new, open a new
buffer for every image. When :multi, insert images one after
another."
  :tag "Control multimedia buffer behavior"
  :group 'julia-snail
  :options '(:single-reuse :single-new :multi)
  :safe (lambda (v) (memq v '(:single-reuse :single-new :multi)))
  :type 'keyword)
(make-variable-buffer-local 'julia-snail-multimedia-buffer-style)

(defcustom julia-snail-company-doc-enable t
  "If company-mode is installed, this flag determines if its documentation integration should be enabled."
  :tag "Control company-mode documentation integration"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)

(defcustom julia-snail-use-emoji-mode-lighter t
  "If true, try to use a snail emoji in the modeline lighter instead of text."
  :tag "Control use of emoji in modeline lighter"
  :group 'julia-snail
  :safe 'booleanp
  :type 'boolean)


;;; --- constants

(defconst julia-snail--julia-files
  (list "JuliaSnail.jl" "Manifest.toml" "Project.toml"))

(defconst julia-snail--julia-files-local
  (mapcar (lambda (f)
            (concat (if load-file-name
                        (file-name-directory load-file-name)
                      (file-name-as-directory default-directory))
                    f))
          julia-snail--julia-files))

(defconst julia-snail--server-file
  (-find (lambda (f)
           (string-equal "JuliaSnail.jl" (file-name-nondirectory f)))
         julia-snail--julia-files-local))


;;; --- variables

(defvar julia-snail-debug nil
  "When t, show more runtime information.")

(defvar-local julia-snail--process nil)

;;; TODO: Maybe this should hash by proc+reqid rather than just reqid?
(defvar julia-snail--requests
  (make-hash-table :test #'equal))

(defvar julia-snail--proc-responses
  (make-hash-table :test #'equal))

(defvar julia-snail--cache-proc-implicit-file-module
  (make-hash-table :test #'equal))

(defvar julia-snail--cache-proc-basedir
  (make-hash-table :test #'equal))

(defvar-local julia-snail--repl-go-back-target nil)

(defvar julia-snail--compilation-regexp-alist
  '(;; matches "while loading /tmp/Foo.jl, in expression starting on line 2"
    (julia-load-error . ("while loading \\([^ ><()\t\n,'\";:]+\\), in expression starting on line \\([0-9]+\\)" 1 2))
    ;; matches "around /tmp/Foo.jl:2", also starting with "at" or "Revise"
    (julia-loc . ("\\(around\\|at\\|Revise\\) \\([^ ><()\t\n,'\";:]+\\):\\([0-9]+\\)" 2 3))
    ;; matches "omitting file /tmp/Foo.jl due to parsing error near line 2", from Revise.parse_source!
    (julia-warn-revise . ("omitting file \\([^ ><()\t\n,'\";:]+\\) due to parsing error near line \\([0-9]+\\)" 1 2))
    )
  "Specifications for highlighting error locations.
Uses function `compilation-shell-minor-mode'.")


;;; --- pre-declarations

(defvar julia-snail-mode)
(defvar julia-snail-repl-mode)


;;; --- Snail protocol request tracking data structure

(cl-defstruct julia-snail--request-tracker
  repl-buf
  originating-buf
  (callback-success (lambda (&optional _data) (message "Snail command succeeded")))
  (callback-failure (lambda () (message "Snail command failed")))
  (display-error-buffer-on-failure? t)
  tmpfile
  tmpfile-local-remote)


;;; --- supporting functions

(defun julia-snail--copy-buffer-local-vars (from-buf)
  "Copy Snail-related buffer-local variables from FROM-BUF to the current buffer."
  (dolist (blv (buffer-local-variables from-buf))
    (let* ((var (car blv))
           (var-name (symbol-name var))
           (val (cdr blv)))
      (when (and (string-prefix-p "julia-snail-" var-name)
                 (not (string-suffix-p "-mode" var-name)))
        (set var val)))))

(defun julia-snail--process-buffer-name (repl-buf)
  "Return the process buffer name for REPL-BUF."
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "No REPL buffer found"))
    (format "%s process" (buffer-name (get-buffer real-buf)))))

(cl-defun julia-snail--message-buffer (repl-buf name message &key (markdown nil))
  "Return a buffer named NAME linked to REPL-BUF containing MESSAGE."
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "No REPL buffer found"))
    (let* ((msg-buf-name (format "%s %s" (buffer-name (get-buffer real-buf)) name))
           (msg-buf (get-buffer-create msg-buf-name)))
      (with-current-buffer msg-buf
        (read-only-mode -1)
        (erase-buffer)
        (insert message)
        (goto-char (point-min))
        (when (and markdown (fboundp 'markdown-mode))
          (defvar markdown-hide-markup)
          (declare-function markdown-mode "markdown-mode.el")
          (declare-function markdown-view-mode "markdown-mode.el")
          (let ((markdown-hide-markup t))
            ;; older versions of markdown-mode do not have markdown-view-mode
            (if (fboundp 'markdown-view-mode)
                (markdown-view-mode)
              (markdown-mode))))
        (read-only-mode 1)
        (julia-snail-message-buffer-mode 1))
      msg-buf)))

;; set error buffer to compilation mode, so that one may directly jump to the relevant files
;; adapted from julia-repl by Tamas Papp
(defun julia-snail--setup-compilation-mode (message-buffer basedir)
  "Setup compilation mode for the the current buffer in MESSAGE-BUFFER.
BASEDIR is used for resolving relative paths."
  (with-current-buffer message-buffer
    (setq-local compilation-error-regexp-alist-alist
                julia-snail--compilation-regexp-alist)
    (setq-local compilation-error-regexp-alist
                (mapcar #'car compilation-error-regexp-alist-alist))
    (compilation-mode)
    (when basedir
      (setq-local compilation-search-path (list basedir)))))

(defun julia-snail--flash-region (start end)
  "Highlight the region outlined by START and END for TIMEOUT period."
  (pulse-momentary-highlight-region start end 'highlight))

(defun julia-snail--construct-module-path (module)
  "Return a Julia array representing the module path of MODULE as Julia symbols.
MODULE can be:
- nil, which returns [:Main]
- an Elisp keyword, which returns [<keyword>], including the
  leading colon in the keyword
- an Elisp list, which can contain either keywords or strings,
  and which is converted to a Julia array literal with the
  entries of the input list converted to Julia keywords"
  (cond ((null module) "[:Main]")
        ((keywordp module) (format "[%s]" module))
        ((listp module) (format
                         "[%s]"
                         (s-join " " (-map (lambda (s)
                                             (if (keywordp s)
                                                 (format "%s" s)
                                               (format ":%s" s)))
                                           module))))
        (t (error "Malformed module specification"))))

(defmacro julia-snail--with-syntax-table (&rest body)
  "Evaluate BODY with a Snail-specific syntax table."
  (declare (indent defun))
  `(let ((stab (copy-syntax-table)))
     (with-syntax-table stab
       (modify-syntax-entry ?. "_")
       (modify-syntax-entry ?@ "_")
       (modify-syntax-entry ?= " ")
       (modify-syntax-entry ?$ " ")
       ,@body)))

(defun julia-snail--bslash-before-p (pos)
  (when-let (c (char-before pos))
    (char-equal c ?\\)))

(defun julia-snail--identifier-at-point ()
  "Return identifier at point using Snail-specific syntax table."
  (julia-snail--with-syntax-table
    (let ((identifier (thing-at-point 'symbol t))
          (start (car (bounds-of-thing-at-point 'symbol))))
      (if (julia-snail--bslash-before-p start)
          (concat "\\" identifier)
        identifier))))

(defun julia-snail--identifier-at-point-bounds ()
  "Return the bounds of the identifier at point using Snail-specific syntax table."
  (julia-snail--with-syntax-table
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (julia-snail--bslash-before-p (car bounds))
          `(,(- (car bounds) 1) . ,(cdr bounds))
        bounds))))

(defmacro julia-snail--wait-while (condition increment maximum)
  "Synchronously wait as long as CONDITION evaluates to true.
INCREMENT: polling frequency, ms.
MAXIMUM: max timeout, ms.
Returns nil if the poll timed out, t otherwise."
  (let ((sleep-total (gensym))
        (incr (gensym))
        (max (gensym)))
    `(let ((,sleep-total 0.0)
           ;; convert arguments from milliseconds to seconds for sit-for
           (,incr (/ ,increment 1000.0))
           (,max (/ ,maximum 1000.0)))
       (while (and (< ,sleep-total ,max) ,condition)
         ;; XXX: This MUST be sleep-for, not sit-for. sit-for is interrupted by
         ;; input, which breaks the loop on input, which will inadvertently kill
         ;; the wait.
         (redisplay)
         (sleep-for ,incr)
         (setf ,sleep-total (+ ,sleep-total ,incr)))
       ;; return value: t if wait returned early, nil if it timed out
       (< ,sleep-total ,max))))

(defun julia-snail--capture-basedir (buf)
  (julia-snail--send-to-server
    :Main
    "normpath(joinpath(VERSION <= v\"0.7-\" ? JULIA_HOME : Sys.BINDIR, Base.DATAROOTDIR, \"julia\", \"base\"))"
    :repl-buf buf
    :async nil))

(defun julia-snail-test-file-path (file)
  "Test suite accessory: Return path to FILE in the test area."
  ;; XXX: Obnoxious Elisp path construction.
  (let ((location (file-name-directory (locate-library "julia-snail"))))
    (concat
     (file-name-as-directory
      (concat (if load-file-name
                  (file-name-directory load-file-name)
                (file-name-as-directory location))
              (file-name-as-directory "tests")
              (file-name-as-directory "files")))
     file)))

(defun julia-snail-test-send-buffer-file-sync ()
  "Test suite accessory: Same as julia-snail-send-buffer-file, but synchronous."
  (let ((reqid (julia-snail-send-buffer-file))) ; wait for async result to return
    (julia-snail--wait-while
     (gethash reqid julia-snail--requests) 50 10000)))

(defun julia-snail--encode-base64 (&optional buf)
  (let ((s (with-current-buffer (or buf (current-buffer))
             (encode-coding-string (buffer-string)
                                   buffer-file-coding-system))))
    (base64-encode-string s)))

(defun julia-snail--copy-snail-to-remote-host ()
  (let* (;; checksum all relevant files as one, copy into a directory
         ;; keyed off the checksum if it doesn't already exist (basically cache
         ;; the current version of the Julia code (.jl and .toml files)
         (checksum (with-temp-buffer
                     (cl-loop for f in julia-snail--julia-files-local do
                              (insert-file-contents-literally f))
                     (secure-hash 'sha256 (current-buffer))))
         (snail-remote-dir (concat (file-name-as-directory (temporary-file-directory))
                                   (concat "julia-snail-" checksum "/"))))
    (unless (file-exists-p snail-remote-dir)
      (make-directory snail-remote-dir)
      (cl-loop for f in julia-snail--julia-files-local do
               (copy-file f snail-remote-dir t)))
    snail-remote-dir))

(defun julia-snail--launch-command ()
  (let* ((extra-args (if (listp julia-snail-extra-args)
                         (mapconcat 'identity julia-snail-extra-args " ")
                       julia-snail-extra-args))
	 (remote-method (file-remote-p default-directory 'method))
         (remote-user (file-remote-p default-directory 'user))
         (remote-host (file-remote-p default-directory 'host))
	 (remote-dir-server-file (if (equal nil remote-method)
				     ""
				   (concat (file-remote-p (julia-snail--copy-snail-to-remote-host) 'localname) "JuliaSnail.jl"))))
    (cond
     ;; local REPL
     ((equal nil remote-method)
      (format "%s %s -L %s" julia-snail-executable extra-args julia-snail--server-file))
     ;; remote REPL
     ((string-equal "ssh" remote-method)
      (format "ssh -t -L %1$s:localhost:%2$s %3$s %4$s %5$s -L %6$s"
              julia-snail-port
              (or julia-snail-remote-port julia-snail-port)
              (concat
               (if remote-user (concat remote-user "@") "")
               remote-host)
              julia-snail-executable
              extra-args
              remote-dir-server-file))
     ;; container REPL
     ((string-equal "docker" remote-method)
      (format "docker exec -it %s %s %s -L %s"
	      remote-host
	      julia-snail-executable
	      extra-args
	      remote-dir-server-file)))))

(defun julia-snail--efn (path &optional starting-dir)
  "A variant of expand-file-name that (1) just does
expand-file-name on local files, and (2) returns the expanded
form of the remote path without any host connection string
components. Example: (julia-snail--efn \"/ssh:host:~/file.jl\")
returns \"/home/username/file.jl\"."
  (let* ((expanded (expand-file-name path starting-dir))
         (remote-local-path (file-remote-p expanded 'localname)))
    (if remote-local-path
        remote-local-path
      expanded)))

(defun julia-snail--add-to-perspective (buf)
  (when (and (featurep 'perspective) (bound-and-true-p persp-mode)) ; perspective-el support
    (declare-function persp-add-buffer "perspective.el")
    (persp-add-buffer buf))
  (when (and (featurep 'persp-mode) (bound-and-true-p persp-mode)) ; persp-mode support
    (declare-function persp-add-buffer "persp-mode.el")
    (declare-function get-current-persp "persp-mode.el")
    (persp-add-buffer buf (get-current-persp) nil)))

(defun julia-snail--mode-lighter (&optional extra)
  (let ((snail-emoji (char-from-name "SNAIL")))
    (if (and julia-snail-use-emoji-mode-lighter
             snail-emoji
             (char-displayable-p snail-emoji))
        (format " %c%s" snail-emoji (if extra extra ""))
      (format " Snail%s" (if extra extra "")))))


;;; --- connection management functions

(defun julia-snail--clear-proc-caches (process-buf)
  "Clear connection-specific internal Snail xref, completion, and module caches."
  (when process-buf
    (remhash process-buf julia-snail--cache-proc-implicit-file-module)
    (remhash process-buf julia-snail--cache-proc-basedir)))

(defun julia-snail--repl-cleanup ()
  "REPL buffer cleanup."
  (let ((process-buf (get-buffer (julia-snail--process-buffer-name (current-buffer)))))
    (julia-snail--clear-proc-caches process-buf)
    (when process-buf
      (kill-buffer process-buf)))
  (setq julia-snail--process nil))

(defun julia-snail--repl-enable ()
  "REPL buffer minor mode initializer."
  (add-hook 'kill-buffer-hook #'julia-snail--repl-cleanup nil t)
  (let ((repl-buf (current-buffer))
        (process-buf (get-buffer-create (julia-snail--process-buffer-name (current-buffer)))))
    (julia-snail--add-to-perspective process-buf)
    (with-current-buffer process-buf
      (unless julia-snail--process
        (julia-snail--copy-buffer-local-vars repl-buf)
        ;; XXX: This is currently necessary because there does not appear to be
        ;; a way to pass arguments to an interactive Julia session. This does
        ;; not work: `julia -L JuliaSnail.jl -- $PORT`.
        ;; https://github.com/JuliaLang/julia/issues/10226 refers to this
        ;; problem and supposedly fixes it, but it does not work for me with
        ;; Julia 1.0.4.
        ;; TODO: Follow-up on https://github.com/JuliaLang/julia/issues/33752
        (message "Starting Julia process and loading Snail...")
        ;; XXX: Wait briefly in case the Julia executable failed to launch.
        (with-current-buffer repl-buf
          ;; XXX: This use of julia-snail--wait-while causes a mysterious
          ;; byte-compiler warning saying the result value of the macro is
          ;; unused. Indeed, this is intentional. Plenty of other places in the
          ;; code ignore the return value of julia-snail--wait-while, all
          ;; without causing the byte-compiler to complain.
          (with-no-warnings
            (julia-snail--wait-while
             (not (string-equal "julia>" (current-word)))
             100
             2000)))
        (unless (buffer-live-p repl-buf)
          (user-error "The vterm buffer is inactive; double-check julia-snail-executable path"))
        ;; now try to send the Snail startup command
        (julia-snail--send-to-repl
         (format "JuliaSnail.start(%d%s) ; # please wait, time-to-first-plot..."
		 (or julia-snail-remote-port julia-snail-port)
		 (if (string-equal "docker" (file-remote-p (buffer-file-name julia-snail--repl-go-back-target) 'method))
		     "; addr=\"0.0.0.0\""
		   ""))
          :repl-buf repl-buf
          ;; wait a while in case dependencies need to be downloaded
          :polling-timeout (* 5 60 1000)
          :async nil)
        ;; connect to the server
        (let ((netstream (let ((attempt 0)
                               (max-attempts 5)
                               (stream nil))
                           (while (and (< attempt max-attempts) (null stream))
                             (cl-incf attempt)
                             (message "Snail connecting to Julia process, attempt %d/5..." attempt)
                             (condition-case nil
                                 (setq stream (open-network-stream "julia-process" process-buf "localhost" julia-snail-port))
                               (error (when (< attempt max-attempts)
                                        (sleep-for 0.75)))))
                           stream)))
          (if netstream
              (with-current-buffer repl-buf
                ;; NB: buffer-local variable!
                (setq julia-snail--process netstream)
                (set-process-filter julia-snail--process #'julia-snail--server-response-filter)
                ;; TODO: Implement a sanity check on the Julia environment. Not
                ;; sure how. But a failed dependency load (like CSTParser) will
                ;; leave Snail in a bad state.
                (message "Snail connected to Julia. Happy hacking!")
                ;; Query base directory, and cache
                (puthash process-buf (julia-snail--capture-basedir repl-buf)
                         julia-snail--cache-proc-basedir))
            ;; something went wrong
            (error "Failed to connect to Snail server"))
          ;; post-connection initialization:
          (when netstream
            (when (buffer-local-value 'julia-snail-multimedia-enable repl-buf)
              (julia-snail--send-to-server
                '("JuliaSnail" "Multimedia")
                "display_on()"
                :repl-buf repl-buf
                :async nil))
            ;; other initializations can go here
            ))))))

(defun julia-snail--repl-disable ()
  "REPL buffer minor mode cleanup."
  (julia-snail--repl-cleanup))

(defun julia-snail--enable ()
  "Source buffer minor mode initializer."
  ;; placeholder
  nil
  )

(defun julia-snail--disable ()
  "Source buffer minor mode cleanup."
  ;; placeholder
  nil
  )


;;; --- Julia REPL and Snail server interaction functions

(cl-defun julia-snail--send-to-repl
    (str
     &key
     (repl-buf (get-buffer julia-snail-repl-buffer))
     (async t)
     (polling-interval 20)
     (polling-timeout julia-snail-async-timeout))
  "Insert str directly into the REPL buffer. When :async is nil,
wait for the REPL prompt to return, otherwise return immediately."
  (declare (indent defun))
  (unless repl-buf
    (user-error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer))
  (with-current-buffer repl-buf
    (vterm-send-string str)
    (vterm-send-return))
  (unless async
    ;; wait for the inclusion to succeed (i.e., the prompt prints)
    (julia-snail--wait-while
     (with-current-buffer repl-buf
       (not (string-equal "julia>" (current-word))))
     polling-interval
     polling-timeout)))

(cl-defun julia-snail--send-to-server
    (module
     str
     &key
     (repl-buf (get-buffer julia-snail-repl-buffer))
     (async t)
     (async-poll-interval 20)
     (async-poll-maximum julia-snail-async-timeout)
     (display-error-buffer-on-failure? t)
     callback-success
     callback-failure)
  "Send STR to Snail server, and evaluate it in the context of MODULE.
Run callback-success and callback-failure as appropriate.
When :async is t (default), return the request id. When :async is
nil, wait for the result and return it."
  (declare (indent defun))
  (unless repl-buf
    (user-error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer))
  (let* ((process-buf (get-buffer (julia-snail--process-buffer-name repl-buf)))
         (originating-buf (current-buffer))
         (module-ns (julia-snail--construct-module-path module))
         (reqid (format "%04x%04x" (random (expt 16 4)) (random (expt 16 4))))
         (code-str (json-encode-string str))
         (display-code-str (if julia-snail-debug
                               code-str
                             (s-truncate 80 code-str)))
         (msg (format "(ns = %s, reqid = \"%s\", code = %s)\n"
                      module-ns
                      reqid
                      code-str))
         (display-msg (format "(ns = %s, reqid = \"%s\", code = %s)\n"
                              module-ns
                              reqid
                              display-code-str))
         (res-sentinel (gensym))
         (res res-sentinel))
    (with-current-buffer process-buf
      (goto-char (point-max))
      (insert display-msg))
    (process-send-string process-buf msg)
    (spinner-start 'progress-bar)
    (puthash reqid
             (make-julia-snail--request-tracker
              :repl-buf repl-buf
              :originating-buf originating-buf
              :display-error-buffer-on-failure? display-error-buffer-on-failure?
              :callback-success (lambda (request-info &optional data)
                                  (unless async
                                    (setq res (or data :nothing)))
                                  (when callback-success
                                    (funcall callback-success request-info data)))
              :callback-failure (lambda (request-info)
                                  (unless async
                                    (setq res :nothing))
                                  (when callback-failure
                                    (funcall callback-failure request-info))))
             julia-snail--requests)
    ;; return value logic:
    (if async
        reqid
      ;; XXX: Non-async (i.e. synchronous) server requests need to poll the
      ;; response. This means they can either (1) succeed, (2) timeout, or (3)
      ;; error out. Because errors occur in the process filter function and
      ;; therefore outside the scope of a potential condition-case, they must be
      ;; processed with a non-local transfer of control (throw and catch).
      (let ((wait-result
             (catch 'julia-snail--server-filter-error
               (julia-snail--wait-while (eq res-sentinel res) async-poll-interval async-poll-maximum))))
        ;; wait-result can be t if poll succeeded, nil if it timed out, and an
        ;; error if something blew up. Note that an explicit check for t is
        ;; necessary here because wait-result can be truthy but nevertheless an
        ;; error. This happens if an error value is caught in the `catch'.
        (if (eq t wait-result)
            res
          (let ((error-msg (if (null wait-result)
                               "Snail command timed out"
                             (format "Snail error: %s" wait-result))))
            (when callback-failure
              (funcall callback-failure))
            (with-current-buffer originating-buf
              (spinner-stop))
            (error error-msg)))))))

(cl-defun julia-snail--send-to-server-via-tmp-file
    (module
     str
     filename
     line-num
     &key
     (repl-buf (get-buffer julia-snail-repl-buffer))
     callback-success
     callback-failure)
  "Send STR to server by first writing it to a tmpfile, calling
Julia include on the tmpfile, and then deleting the file. The
code in the tmpfile will be parsed in Julia as if it were
actually located in FILENAME starting at LINE-NUM and will be
evaluated in the context of MODULE."
  (declare (indent defun))
  (let* ((text (concat "begin\n" (s-trim str) "\nend\n"))
         (module-ns (julia-snail--construct-module-path module))
         (tmpfile (make-temp-file
                   (expand-file-name "julia-tmp" ; NOT julia-snail--efn
                                     (or small-temporary-file-directory
                                         (temporary-file-directory)))))
         (tmpfile-local-remote (file-remote-p tmpfile 'localname)))
    (progn
      (with-temp-file tmpfile
        (insert text))
      (let ((reqid (julia-snail--send-to-server
                     :Main
                     (format "Main.JuliaSnail.eval_tmpfile(\"%s\", %s, \"%s\", %s)"
                             (or tmpfile-local-remote tmpfile)
                             module-ns
                             filename
                             line-num)
                     :repl-buf repl-buf
                     ;; TODO: Only async via-tmp-file evaluation is currently
                     ;; supported because we rely on getting the reqid back from
                     ;; julia-snail--send-to-server, and that only happens with
                     ;; (async t). This may or may not be worth fixing in the
                     ;; future.
                     :async t
                     :callback-success callback-success
                     :callback-failure callback-failure)))
        (puthash reqid
                 (make-julia-snail--request-tracker
                  :repl-buf repl-buf
                  :originating-buf (current-buffer)
                  :callback-success callback-success
                  :callback-failure callback-failure
                  :tmpfile tmpfile
                  :tmpfile-local-remote tmpfile-local-remote)
                 julia-snail--requests)
        reqid))))

(defun julia-snail--server-response-filter (proc str)
  "Snail process filter for PROC given input STR; used as argument to `set-process-filter'."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; insert at the end unconditionally
      (goto-char (point-max))
      (insert str)
      (set-marker (process-mark proc) (point))
      ;; Need to read and eval the value sent in by the process (str). But it
      ;; may have been chunked. Assume that a successful read signals the end of
      ;; input, but a failed read needs to be concatenated to other upcoming
      ;; reads. Track them in a table hashed by the proc.
      (let ((candidate (s-concat (gethash proc julia-snail--proc-responses) str)))
        (condition-case err
            (let ((read-str (read candidate)))
              ;; read succeeds, so clean up and return its eval value
              (remhash proc julia-snail--proc-responses)
              ;; scary
              (eval read-str))
          ;; read failed due to end-of-file: this means more data is incoming; continue
          (end-of-file
           (puthash proc candidate julia-snail--proc-responses))
          ;; If an unexpected error occurs at this point, it will have no normal
          ;; condition-case context. Unfortunately, this leaves non-local
          ;; transfer of control as the only way to notify the rest of the
          ;; program that something went haywire.
          (error
           (throw 'julia-snail--server-filter-error err)))))))


;;; --- Snail server response handling functions

(defun julia-snail--response-base (reqid)
  "Snail response handler for REQID, base function."
  (let ((request-info (gethash reqid julia-snail--requests)))
    (when request-info
      ;; tmpfile
      (when-let (tmpfile (julia-snail--request-tracker-tmpfile request-info))
        (delete-file tmpfile))
      ;; stop spinner
      (with-current-buffer (julia-snail--request-tracker-originating-buf request-info)
        (spinner-stop))
      ;; remove request ID from requests hash
      (remhash reqid julia-snail--requests))))

(defun julia-snail--response-success (reqid result-data)
  "Snail success response handler for REQID given RESULT-DATA."
  (let* ((request-info (gethash reqid julia-snail--requests))
         (callback-success (julia-snail--request-tracker-callback-success request-info)))
    (when callback-success
      (funcall callback-success request-info result-data)))
  (julia-snail--response-base reqid))

(defun julia-snail--response-failure (reqid error-message error-stack)
  "Snail failure response handler for REQID, display ERROR-MESSAGE and ERROR-STACK."
  (if (not julia-snail-show-error-window)
      (message error-message)
    (let* ((request-info (gethash reqid julia-snail--requests))
           (repl-buf (julia-snail--request-tracker-repl-buf request-info))
           (process-buf (get-buffer (julia-snail--process-buffer-name repl-buf)))
           (error-buffer (julia-snail--message-buffer
                          repl-buf
                          "error"
                          (format "%s\n\n%s" error-message (s-join "\n" error-stack))))
           (callback-failure (julia-snail--request-tracker-callback-failure request-info)))
      (when (julia-snail--request-tracker-display-error-buffer-on-failure? request-info)
        (julia-snail--setup-compilation-mode error-buffer (gethash process-buf julia-snail--cache-proc-basedir))
        (pop-to-buffer error-buffer))
      (when callback-failure
        (funcall callback-failure request-info))))
  (julia-snail--response-base reqid))


;;; --- CST parser interface

(defun julia-snail--cst-module-at (buf pt)
  (let* ((byteloc (position-bytes pt))
         (encoded (julia-snail--encode-base64 buf))
         (res (julia-snail--send-to-server
                :Main
                (format "JuliaSnail.CST.moduleat(\"%s\", %d)" encoded byteloc)
                :async nil)))
    (if (eq res :nothing)
        nil
      res)))

(defun julia-snail--cst-block-at (buf pt)
  (let* ((byteloc (position-bytes pt))
         (encoded (julia-snail--encode-base64 buf))
         (res (julia-snail--send-to-server
                :Main
                (format "JuliaSnail.CST.blockat(\"%s\", %d)" encoded byteloc)
                :async nil)))
    (if (eq res :nothing)
        nil
      res)))

(defun julia-snail--cst-includes (buf)
  (let* ((encoded (julia-snail--encode-base64 buf))
         (pwd (file-name-directory (julia-snail--efn (buffer-file-name buf))))
         (res (julia-snail--send-to-server
                :Main
                (format "JuliaSnail.CST.includesin(\"%s\", \"%s\")" encoded pwd)
                :async nil))
         (includes (make-hash-table :test #'equal)))
    (unless (eq res :nothing)
      (cl-loop for (file modules) in (-partition 2 res) do
               (puthash file modules includes)))
    ;; TODO: Maybe there's a situation in which returning :error is appropriate?
    includes))


;;; --- Julia module tracking implementation

(defun julia-snail--module-merge-includes (current-filename includes)
  "Update file module cache using INCLUDES tree parsed from CURRENT-FILENAME."
  (let* ((process-buf (get-buffer (julia-snail--process-buffer-name julia-snail-repl-buffer)))
         (proc-includes (or (gethash process-buf
                                     julia-snail--cache-proc-implicit-file-module)
                            (puthash process-buf (make-hash-table :test #'equal)
                                     julia-snail--cache-proc-implicit-file-module)))
         (current-file-module (gethash (julia-snail--efn current-filename) proc-includes)))
    ;; merge includes with the proc-includes table
    (cl-loop for included-file being the hash-keys of includes using (hash-values included-file-modules) do
             (puthash included-file
                      (if current-file-module
                          (append current-file-module included-file-modules)
                        included-file-modules)
                      proc-includes))
    ;; done
    proc-includes))

(defun julia-snail--module-for-file (file)
  "Retrieve the module for FILE from `julia-snail--cache-proc-implicit-file-module' table."
  (let* ((filename (julia-snail--efn file))
         (process-buf (get-buffer (julia-snail--process-buffer-name julia-snail-repl-buffer)))
         (proc-includes (gethash process-buf julia-snail--cache-proc-implicit-file-module
                                 (make-hash-table :test #'equal)))
         (parent-modules (gethash filename proc-includes (list))))
    parent-modules))

(defun julia-snail--module-at-point (&optional partial-module)
  "Return the current Julia module at point as an Elisp list, including PARTIAL-MODULE if given."
  (let ((partial-module (or partial-module
                            (julia-snail--cst-module-at (current-buffer) (point))))
        (module-for-file (julia-snail--module-for-file (buffer-file-name (buffer-base-buffer)))))
    (or (if module-for-file
            (append module-for-file partial-module)
          partial-module)
        '("Main"))))


;;; --- xref implementation

(defun julia-snail-xref-backend ()
  "Emacs xref API."
  'xref-julia-snail)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-julia-snail)))
  "Emacs xref API."
  (julia-snail--identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-julia-snail)))
  "Emacs xref API."
  (let* ((module (julia-snail--module-at-point))
         (ns (s-join "." module)))
    (julia-snail--send-to-server
      module
      (format "Main.JuliaSnail.lsnames(%s, all=true, imported=true, include_modules=false, recursive=true)" ns)
      :async nil)))

(defun julia-snail--make-xrefs-helper (response)
  "Emacs xref API helper for RESPONSE."
  (if (or (null response) (eq :nothing response))
      nil
    (mapcar (lambda (candidate)
              (let* ((descr (-first-item candidate))
                     (path (-second-item candidate))
                     (line (-third-item candidate))
                     ;; convert to Tramp path when working with a remote REPL
                     (tramp-prefix (file-remote-p default-directory))
                     (real-path (if tramp-prefix
                                    (concat tramp-prefix path)
                                  path)))
                (xref-make descr
                           (if (file-exists-p real-path)
                               (xref-make-file-location real-path line 0)
                             (xref-make-bogus-location "xref location not found")))))
            response)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-julia-snail)) identifier)
  "Emacs xref API."
  (unless identifier
    (user-error "No identifier at point"))
  (let* ((module (julia-snail--module-at-point))
         ;; Grab everything in the identifier up to the last dot, i.e., the
         ;; fully-qualified module name, and everything after the last dot,
         ;; which should be the symbol in the module.
         (identifier-split (save-match-data
                             (if (string-match
                                  "\\(.*\\)\\.\\(.*\\)"
                                  identifier)
                                 (list (match-string 1 identifier)
                                       (match-string 2 identifier))
                               (list module identifier))))
         (identifier-ns (-first-item identifier-split))
         (identifier-ns-real (cond ((listp identifier-ns)
                                    (-last-item identifier-ns))
                                   ((null identifier-ns)
                                    "Main")
                                   (t
                                    identifier-ns)))
         (identifier-name (-second-item identifier-split))
         (res (julia-snail--send-to-server
                module
                (format "Main.JuliaSnail.lsdefinitions(%s, \"%s\")"
                        identifier-ns-real identifier-name)
                :async nil)))
    (julia-snail--make-xrefs-helper res)))

;;; TODO: Implement this. See
;;; https://discourse.julialang.org/t/finding-uses-of-a-method/32729/3 for
;;; information about how it can be done. Key points: (1) It is most reliable
;;; for executed code, which is of course a non-starter for IDE functionality.
;;; (2) It can be done by iterating through all methods in all modules and
;;; calling Base.uncompressed_ast and looking for appropriate calls. Seems like
;;; it won't be accurate for functions called through indirection, but would
;;; definitely be a step in the right direction.
(cl-defmethod xref-backend-references ((_backend (eql xref-julia-snail)) _identifier)
  nil)

(cl-defmethod xref-backend-apropos ((_backend (eql xref-julia-snail)) pattern)
  (let ((res (julia-snail--send-to-server
               :Main
               (format "Main.JuliaSnail.apropos(%s, \"%s\")"
                       "Main"
                       pattern)
               :async nil)))
    (julia-snail--make-xrefs-helper res)))


;;; --- completion implementation

(defun julia-snail--repl-completions (identifier)
  (let* ((module (julia-snail--module-at-point))
         (res (julia-snail--send-to-server
                :Main
                (format "try; JuliaSnail.replcompletion(\"%1$s\", %2$s); catch; JuliaSnail.replcompletion(\"%1$s\", Main); end"
                        identifier
                        (s-join "." module))
                :async nil)))
    (if (eq :nothing res)
        (list)
      res)))

(defun julia-snail-repl-completion-at-point ()
  "Implementation for Emacs `completion-at-point' system using REPL.REPLCompletions as the provider."
  (let ((identifier (julia-snail--identifier-at-point))
        (bounds (julia-snail--identifier-at-point-bounds))
        (split-on "\\.")
        (prefix "")
        start)
    (when bounds
      ;; If identifier starts with a backslash we need to add an extra "\\" to
      ;; make sure that the string which arrives to the completion provider on the server starts with "\\".
      (when (s-equals-p (substring identifier 0 1) "\\")
        (setq prefix "\\"))
      ;; check if identifier at point is inside a string and attach the opening quotes so
      ;; we get path completion.
      (when-let (prev (char-before (car bounds)))
        (when (char-equal prev ?\")
          (setq identifier (concat "\\\"" identifier))
          ;; TODO: add support for Windows paths (splitting on "\\" when appropriate)
          (setq split-on "/")))
      ;; If identifier is not a string, we split on "." so that completions of
      ;; the form Module.f -> Module.func work (since
      ;; `julia-snail--repl-completions' will return only "func" in this case)
      (setq start (- (cdr bounds) (length (car (last (s-split split-on identifier))))))
      (list start
            (cdr bounds)
            (completion-table-dynamic
             (lambda (_) (julia-snail--repl-completions (concat prefix identifier))))
            :exclusive 'no))))


;;; --- company-mode support

(defun julia-snail--company-doc-buffer (str)
  (let* ((module (julia-snail--module-at-point))
         (name (s-concat (s-join "." module) "." str))
         (doc (julia-snail--send-to-server
                :Main
                (format "@doc %s" name)
                :display-error-buffer-on-failure? nil
                :async nil)))
    (let ((buf (julia-snail--message-buffer
                julia-snail-repl-buffer
                "doc-buffer"
                (if (eq :nothing doc)
                    "Documentation not found!\nDouble-check your package activation and imports."
                  doc)
                :markdown nil)))
      (with-current-buffer buf
        (julia-snail--add-to-perspective buf)
        (font-lock-ensure))
      buf)))

(defun julia-snail-company-capf ()
  (interactive)
  (let* ((comp (julia-snail-repl-completion-at-point))
         (doc (list :company-doc-buffer
                    #'julia-snail--company-doc-buffer)))
    (cl-concatenate 'list comp doc)))


;;; --- eldoc implementation

(defun julia-snail-eldoc ()
  "Implementation for ElDoc."
  ;; TODO: Implement something reasonable. This is pretty tricky to do in a
  ;; world of generic functions, since the parser will need to do the work of
  ;; figuring out just which possible signatures of a function are being called
  ;; and display documentation accordingly.
  nil
)


;;; --- multimedia support
;;; Adapted from a PR by https://github.com/dahtah (https://github.com/gcv/julia-snail/pull/21).

(defun julia-snail-multimedia-display (img)
  (let* ((repl-buf (get-buffer julia-snail-repl-buffer))
         (style (buffer-local-value 'julia-snail-multimedia-buffer-style repl-buf))
         (mm-buf-name-base (format "%s mm" (buffer-name repl-buf)))
         (mm-buf-name (if (memq style '(:single-reuse :multi))
                          mm-buf-name-base
                        (generate-new-buffer-name mm-buf-name-base)))
         (mm-buf (get-buffer-create mm-buf-name))
         (decoded-img (base64-decode-string img)))
    (with-current-buffer mm-buf
      ;; allow directly-inserted images to be erased
      (fundamental-mode)
      (read-only-mode -1)
      (when (eq :single-reuse style)
        (erase-buffer))
      (when (memq style '(:single-reuse :single-new))
        ;; use image-mode
        (insert decoded-img)
        (image-mode))
      (when (eq :multi style)
        ;; insert images as objects
        ;; switching from previously-used :single-reuse requires special cleanup
        (when (eq 'image-mode major-mode)
          (erase-buffer)
          (fundamental-mode))
        ;; check buffer size and insert separator as needed
        (when (> (buffer-size) 0)
          (goto-char (point-max))
          (insert "\n"))
        (if (image-type-available-p 'imagemagick)
            (let ((shortest (car
                             (-sort
                              (lambda (a b)
                                (< (window-height a)
                                   (window-height b)))
                              (get-buffer-window-list mm-buf)))))
              (if shortest
                  (insert-image (create-image decoded-img 'imagemagick t :height (round (* 0.80 (window-pixel-height shortest)))))
                (insert-image (create-image decoded-img 'imagemagick t))))
          (insert-image (create-image decoded-img nil t)))
        (insert "\n"))
      (dolist (win (get-buffer-window-list mm-buf))
        (set-window-point win (point-max)))
      (read-only-mode 1)
      (julia-snail-multimedia-buffer-mode 1))
    (display-buffer mm-buf)
    (when julia-snail-multimedia-buffer-autoswitch
      (pop-to-buffer mm-buf))))

(defun julia-snail-multimedia-toggle-display-in-emacs ()
  "Turn Julia multimedia display in Emacs off or on."
  (interactive)
  (unless (display-images-p)
    (user-error "This Emacs display does not support images"))
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (message
     (julia-snail--send-to-server
       '("JuliaSnail" "Multimedia")
       "display_toggle()"
       :repl-buf repl-buf
       :async nil))))


;;; --- commands

;;;###autoload
(defun julia-snail ()
  "Start a Julia REPL and connect to it, or switch if one already exists.
The following buffer-local variables control it:
- `julia-snail-repl-buffer' (default: *julia*)
- `julia-snail-port' (default: 10011)
To create multiple REPLs, give these variables distinct values (e.g.:
*julia my-project-1* and 10012)."
  (interactive)
  (let ((source-buf (when (eq 'julia-mode major-mode) (current-buffer)))
        (repl-buf (get-buffer julia-snail-repl-buffer)))
    (if repl-buf
        (progn
          (setf (buffer-local-value 'julia-snail--repl-go-back-target repl-buf) source-buf)
          (pop-to-buffer repl-buf))
      ;; run Julia in a vterm and load the Snail server file
      (let ((vterm-shell (julia-snail--launch-command))
            ;; XXX: Allocate a buffer for the vterm. When a remote REPL is being
            ;; started, bind the vterm buffer's default-directory to the user's
            ;; home because if (1) a remote REPL is being started,
            ;; default-directory may be remote, and (2) Tramp may notice this,
            ;; mess with the path, and run ssh incorrectly.
            (vterm-buf (let ((default-directory (if (file-remote-p default-directory)
                                                    (expand-file-name "~")
                                                  default-directory)))
                         (generate-new-buffer julia-snail-repl-buffer))))
        (pop-to-buffer vterm-buf)
        (with-current-buffer vterm-buf
          ;; XXX: Set the error color to red to work around breakage relating to
          ;; some color themes and terminal combinations, see
          ;; https://github.com/gcv/julia-snail/issues/11
          (let ((process-environment (append '("JULIA_ERROR_COLOR=red") process-environment)))
            (vterm-mode))
          (when source-buf
            (julia-snail--copy-buffer-local-vars source-buf)
            (setq julia-snail--repl-go-back-target source-buf))
          (julia-snail-repl-mode))))))

(defun julia-snail-send-line ()
  "Copy the line at the current point into the REPL and run it.
This is not module-context aware."
  (interactive)
  (let ((line (s-trim (thing-at-point 'line t))))
    (julia-snail--send-to-repl line)
    (julia-snail--flash-region (point-at-bol) (point-at-eol))))

(defun julia-snail-send-dwim ()
  "Send region, block, or line to Julia REPL."
  (interactive)
  (if (use-region-p)                    ; region
      (julia-snail-send-region)
    (condition-case _err                ; block
        (julia-snail-send-top-level-form)
      (user-error                       ; block fails, so send line
       (julia-snail-send-line)))))

(defun julia-snail-send-code-cell (block-start block-end)
  "Send the current code cell to the Julia REPL and run it in the context of the current module.
Code cells is a notebook-style feature implemented with
https://github.com/astoff/code-cells.el. code-cells-mode must be
enabled for this to work, and something like this is required for
activation:
(add-to-list 'code-cells-eval-region-commands '(julia-snail-mode . julia-snail-send-code-cell))"
  (let* ((text (buffer-substring-no-properties block-start block-end))
         (filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (module (if current-prefix-arg :Main (julia-snail--module-at-point)))
         (line-num (line-number-at-pos block-start)))
    (julia-snail--send-to-server-via-tmp-file
      module
      text
      filename
      line-num
      :callback-success (lambda (_request-info &optional data)
                          (message "code cell evaluated: %s, module %s"
                                   data
                                   (julia-snail--construct-module-path module))))))

(defun julia-snail-send-buffer-file ()
  "Send the current buffer's file into the Julia REPL, and include() it.
This will occur in the context of the Main module, just as it would at the REPL."
  (interactive)
  (let* ((jsrb-save julia-snail-repl-buffer) ; save for callback context
         (filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (module (or (julia-snail--module-for-file filename) '("Main")))
         (includes (julia-snail--cst-includes (current-buffer))))
    (when (or (not (buffer-modified-p))
              (y-or-n-p (format "'%s' is not saved, send to Julia anyway? " filename)))
      (julia-snail--send-to-server
        module
        (format "include(\"%s\"); Main.JuliaSnail.elexpr(true)" filename)
        :callback-success (lambda (_request-info &optional _data)
                            ;; julia-snail-repl-buffer must be rebound here from
                            ;; jsrb-save, because the callback will run in a
                            ;; different scope, in which the correct binding of
                            ;; julia-snail-repl-buffer will have disappeared
                            (let* ((julia-snail-repl-buffer jsrb-save)
                                   (repl-buf (get-buffer julia-snail-repl-buffer)))
                              ;; NB: At the moment, julia-snail--cst-includes
                              ;; does not return :error. However, it might in
                              ;; the future, and this code will then be useful.
                              (if (eq :error includes)
                                  (let ((error-buffer
                                         (julia-snail--message-buffer
                                          repl-buf
                                          "error"
                                          (concat filename
                                                  " loaded in Julia, but the Snail parser failed.\n\n"
                                                  "Please report this as a parser bug:\n\n"
                                                  "https://github.com/gcv/julia-snail/issues\n\n"
                                                  "Please try to narrow down the code which Snail fails to parse.\n"
                                                  "The easiest way of doing this is to bisect the failing source file by\n"
                                                  "commenting out successive halves.\n"
                                                  "The more information about code which Snail cannot parse you include in the bug\n"
                                                  "report, the easier it will be to fix."))))
                                    (pop-to-buffer error-buffer))
                                ;; successful load
                                (julia-snail--module-merge-includes filename includes)
                                (message "%s loaded: module %s"
                                         filename
                                         (julia-snail--construct-module-path module)))))))))

(defun julia-snail-send-region ()
  "Send the region (requires transient-mark) to the Julia REPL and evaluate it.
Normally, this occurs in the context of the current module.
If a prefix arg is used, this instead occurs in the context of Main."
  (interactive)
  (if (null (use-region-p))
      (user-error "No region selected")
    (let* ((block-start (region-beginning))
           (block-end (region-end))
           (text (buffer-substring-no-properties block-start block-end))
           (filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
           (module (if current-prefix-arg :Main (julia-snail--module-at-point)))
           (line-num (line-number-at-pos block-start)))
      (julia-snail--send-to-server-via-tmp-file
        module
        text
        filename
        line-num
        :callback-success (lambda (_request-info &optional data)
                            (message "Selected region evaluated: %s, module %s"
                                     data
                                     (julia-snail--construct-module-path module)))))))

(defun julia-snail-send-top-level-form ()
  "Send the top level form around the point to the Julia REPL and evaluate it.
This occurs in the context of the current module.
Currently only works on blocks terminated with `end'."
  (interactive)
  (let* ((q (julia-snail--cst-block-at (current-buffer) (point)))
         (filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (module (julia-snail--module-at-point (-first-item q)))
         (block-start (byte-to-position (or (-second-item q) -1)))
         (block-end (byte-to-position (or (-third-item q) -1)))
         (top-level-form-name (or (-fourth-item q) nil))
         (line-num (line-number-at-pos block-start))
         (text (condition-case nil
                   (buffer-substring-no-properties block-start block-end)
                 (error ""))))
    (if (null q)
        (user-error "No top-level form at point")
      (julia-snail--flash-region block-start block-end)
      (julia-snail--send-to-server-via-tmp-file
        module
        text
        filename
        line-num
        :callback-success (lambda (_request-info &optional _data)
                            (message "Top-level form evaluated: %s, module %s"
                                     (if top-level-form-name
                                         top-level-form-name
                                       "unknown")
                                     (julia-snail--construct-module-path module)))))))

(defun julia-snail-package-activate (dir)
  "Activate a Pkg project located in DIR in the Julia REPL."
  (interactive "DProject directory: ")
  (let ((expanded-dir (julia-snail--efn dir)))
    (julia-snail--send-to-server
      :Main
      (format "Pkg.activate(\"%s\")" expanded-dir)
      :callback-success (lambda (_request-info &optional _data)
                          (message "Package activated: %s" expanded-dir)))))

(defun julia-snail-doc-lookup (identifier)
  "Look up Julia documentation for symbol at point (IDENTIFIER)."
  (interactive (list (read-string
                      "Documentation look up: "
                      (unless current-prefix-arg (julia-snail--identifier-at-point)))))
  (let* ((module (julia-snail--module-at-point))
         (name (s-concat (s-join "." module) "." identifier))
         (doc (julia-snail--send-to-server
                :Main
                (format "@doc %s" name)
                :display-error-buffer-on-failure? nil
                :async nil)))
    (pop-to-buffer (julia-snail--message-buffer
                    julia-snail-repl-buffer
                    (format "documentation: %s" identifier)
                    (if (eq :nothing doc)
                        "Documentation not found!\nDouble-check your package activation and imports."
                      doc)
                    :markdown t))))

(defun julia-snail-repl-go-back ()
  "Return to a source buffer from a Julia REPL buffer."
  (interactive)
  (when (bound-and-true-p julia-snail--repl-go-back-target)
    (pop-to-buffer julia-snail--repl-go-back-target)))

(defun julia-snail-repl-vterm-kill-line ()
  "Make kill-line (C-k by default) save content to the kill ring."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(defun julia-snail-clear-caches ()
  "Clear connection-specific internal Snail xref, completion, and module caches.
Useful if something seems to wrong."
  (interactive)
  (when (or julia-snail-mode julia-snail-repl-mode)
    (let ((process-buf (get-buffer (julia-snail--process-buffer-name
                                    (if julia-snail-mode
                                        julia-snail-repl-buffer
                                      (current-buffer))))))
      (julia-snail--clear-proc-caches process-buf))))

(defun julia-snail-update-module-cache ()
  "Update cache of implicit modules referenced in current source file.
This is not necessary when files are loaded into the Julia
environment using `julia-snail-send-buffer-file', but it is
useful for a workflow using Revise.jl. It makes xref and
autocompletion aware of the available modules."
  (interactive)
  (let* ((filename (julia-snail--efn (buffer-file-name (buffer-base-buffer))))
         (module (or (julia-snail--module-for-file filename) '("Main")))
         (includes (julia-snail--cst-includes (current-buffer))))
    (julia-snail--module-merge-includes filename includes)
    (message "Caches updated: parent module %s"
             (julia-snail--construct-module-path module))))


;;; --- keymaps

(defvar julia-snail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'julia-snail)
    (define-key map (kbd "C-c C-a") #'julia-snail-package-activate)
    (define-key map (kbd "C-c C-d") #'julia-snail-doc-lookup)
    (define-key map (kbd "C-c C-c") #'julia-snail-send-top-level-form)
    (define-key map (kbd "C-M-x") #'julia-snail-send-top-level-form)
    (define-key map (kbd "C-c C-r") #'julia-snail-send-region)
    (define-key map (kbd "C-c C-l") #'julia-snail-send-line)
    (define-key map (kbd "C-c C-e") #'julia-snail-send-dwim)
    (define-key map (kbd "C-c C-k") #'julia-snail-send-buffer-file)
    (define-key map (kbd "C-c C-m u") #'julia-snail-update-module-cache)
    map))

(defvar julia-snail-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'julia-snail-repl-go-back)
    (define-key map (kbd "C-k") #'julia-snail-repl-vterm-kill-line)
    map))


;;; --- mode definitions

;;;###autoload
(define-minor-mode julia-snail-mode
  "A minor mode for interactive Julia development. Should only be turned on in source buffers."
  :init-value nil
  :lighter (:eval (julia-snail--mode-lighter))
  :keymap julia-snail-mode-map
  (when (eq 'julia-mode major-mode)
    (if julia-snail-mode
        ;; activate
        (progn
          (julia-snail--enable)
          (add-hook 'xref-backend-functions #'julia-snail-xref-backend nil t)
          (add-function :before-until (local 'eldoc-documentation-function) #'julia-snail-eldoc)
          (if (and (featurep 'company)
                   julia-snail-company-doc-enable)
              (add-hook 'completion-at-point-functions #'julia-snail-company-capf nil t)
            (add-hook 'completion-at-point-functions #'julia-snail-repl-completion-at-point nil t)))
      ;; deactivate
      (if (and (featurep 'company)
               julia-snail-company-doc-enable)
          (remove-hook 'completion-at-point-functions #'julia-snail-company-capf t)
        (remove-hook 'completion-at-point-functions #'julia-snail-repl-completion-at-point t))
      (remove-function (local 'eldoc-documentation-function) #'julia-snail-eldoc)
      (remove-hook 'xref-backend-functions #'julia-snail-xref-backend t)
      (julia-snail--disable))))

;;;###autoload
(define-minor-mode julia-snail-repl-mode
  "A minor mode for interactive Julia development. Should only be
turned on in REPL buffers."
  :init-value nil
  :lighter (:eval (julia-snail--mode-lighter))
  :keymap julia-snail-repl-mode-map
  (when (eq 'vterm-mode major-mode)
    (if julia-snail-repl-mode
        (julia-snail--repl-enable)
      (julia-snail--repl-disable))))

(define-minor-mode julia-snail-message-buffer-mode
  "A minor mode for displaying messages returned from the Julia REPL."
  :init-value nil
  :lighter (:eval (julia-snail--mode-lighter " Message"))
  :keymap '(((kbd "q") . quit-window)))

(define-minor-mode julia-snail-multimedia-buffer-mode
  "A minor mode for displaying Julia multimedia output an Emacs buffer."
  :init-value nil
  :lighter (:eval (julia-snail--mode-lighter " MM"))
  :keymap '(((kbd "q") . quit-window)))


;;; --- done

(provide 'julia-snail)

;;; julia-snail.el ends here
