(require 'company)

(defconst sample-completions
  '("function" "struct" "Array" "RandomForest"))

(defun company-snail-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-snail-backend))
    (prefix (and (eq major-mode 'fundamental-mode)
                 (julia-snail--identifier-at-point)))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      sample-completions))))

(with-current-buffer (get-buffer "test.jl")
(setq company-backends '(company-capf)))

(defun snail-ident-at-point ()
    (interactive)
  (message (julia-snail--identifier-at-point))
  )

(snail-ident-at-point)
