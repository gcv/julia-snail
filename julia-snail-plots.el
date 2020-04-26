(define-minor-mode julia-snail-plot-mode
  "A minor mode for displaying a Julia plot inside an Emacs buffer."
  :init-value nil
  :lighter "Julia Plot"
  :keymap '(((kbd "q") . julia-snail--quit-plot-window)))

(defcustom julia-snail-single-plot t
  "If true, plots are showed one-by-one. If false, plots are inserted successively in the plotting buffer"
  :tag "Julia plotting"
  :group 'julia-snail
  :type 'boolean)

(defvar julia-snail--plotting nil)

(defun julia-snail--draw-plot (im encoded)
  (if julia-snail-single-plot
      (julia-snail--show-im im encoded)
    (julia-snail--insert-im im encoded)
    )
  )


(defun julia-snail--init-plotting (buf)
  (progn
    (server-start)
      (julia-snail--send-to-server
        :Main
        "pushdisplay(JuliaSnail.EmacsDisplay());"
        :repl-buf buf
        :async nil
        :callback-success (lambda (&optional _data)
                            (progn
                              (message "Plotting inside Emacs turned on")
                              (setq julia-snail--plotting t)
                              )) )))

(defun julia-snail--cancel-plotting (buf)
  (julia-snail--send-to-server
    :Main
    "popdisplay();"
    :repl-buf buf
    :async nil
    :callback-success (lambda (&optional _data)
                        (progn
                          (message "Plotting inside Emacs turned off")
                          (setq julia-snail--plotting nil)
                          )) ))



(defun julia-snail-plot-in-emacs ()
"Turn on/off plotting in emacs"
    (interactive)
    (if julia-snail--plotting
        (julia-snail--cancel-plotting (current-buffer))
        (julia-snail--init-plotting (current-buffer))
    ))


(defun julia-snail--show-trace (trace)
  (let* ((repl-buf (get-buffer julia-snail-repl-buffer))
         (msg (format "%s\n\n%s" "Stacktrace:" (s-join "\n" trace)))
         (error-buffer (julia-snail--message-buffer repl-buf "Julia error" msg))
         (process-buf (get-buffer (julia-snail--process-buffer-name repl-buf)))
         )
    (julia-snail--setup-compilation-mode error-buffer (gethash process-buf julia-snail--cache-proc-basedir))
    (pop-to-buffer error-buffer)
  ))



(defun julia-snail--show-im (im decode)
  (interactive)
  (let ((buf (get-buffer-create "*julia plot*")))
    (with-current-buffer buf
      (fundamental-mode)
      (message "plotting")

      (setq buffer-read-only nil)

    (erase-buffer)
    (insert im)
    (if decode (base64-decode-region (point-min) (point-max)))
    (pop-to-buffer buf)
    (image-mode)
    (julia-snail-plot-mode)
    )))

(defun julia-snail--quit-plot-window ()
  (interactive)
  (progn
    (with-current-buffer julia-snail-repl-buffer
      (goto-char (point-max))
      )
    (pop-to-buffer julia-snail-repl-buffer)
    ))



(defun julia-snail--insert-im (im decode)
  (interactive)
  (progn
    (if decode (setq im (base64-decode-string im)))

    (let ((buf (get-buffer-create "*julia plots*")) (img (create-image im nil t)))
    (with-current-buffer buf
      (goto-char (point-max))
      (unless  (= (point-max) (point-min))
        (progn
          (insert (propertize "      \n" 'face 'underline))
          (insert "\n")
          ))
      (insert-image img "julia plot")
      (insert-char ?\n 2)
      (pop-to-buffer buf)
      )
    ))
  )


(provide 'julia-snail-plots)

