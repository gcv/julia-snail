(defcustom julia-snail-single-plot t
  "If true, plots are showed one-by-one. If false, plots are inserted successively in the plotting buffer"
  :tag "Julia plotting"
  :group 'julia-snail
  :type 'boolean)

(defvar julia-snail--plotting nil)
(defun julia-snail--draw-plot (im)
  (if julia-snail-single-plot
      (julia-snail--show-svg im)
    (julia-snail--insert-svg im)
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


(defun julia-snail--show-svg (im)
  (interactive)
  (let ((buf (get-buffer-create "*julia plot*")))
    (with-current-buffer buf
    (fundamental-mode)
    (setq buffer-read-only nil) 
    (erase-buffer)
    (insert im)
    (pop-to-buffer buf)
    (image-mode)
    ))
  )

(defun julia-snail--insert-svg (im)
  (interactive)
  (let ((buf (get-buffer-create "*julia plots*")) (img (create-image im 'svg t)))
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
    )
  )


(provide 'julia-snail-plots)

(defun my-overline ()
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (overline (make-overlay beg end))
         (str (propertize
               ;;  a space with one pixel height
               (concat (propertize "\s" 'display '(space :height (1)))
                       ;; visible content alone should determine the line height
                       (propertize "\n" 'line-height t))
               'face '(:background "gray"))))
    ;; delete overlay if containing text is removed from buffer
    (overlay-put overline 'evaporate t)
    ;; place the single pixel newline before to look like an overline
    (overlay-put overline 'before-string str)
    overline))

(defun insert-png (im)
  (interactive)
  (let ((buf (get-buffer-create "*julia plots*")) (img (create-image im 'png t)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert-image img)
      (pop-to-buffer buf)
      )
    )
  )

