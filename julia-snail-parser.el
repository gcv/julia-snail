(require 'parsec)


(defun jsp-whitespace ()
  (parsec-many-as-string
   (parsec-re "[[:space:]\r\n]")))

(defun jsp-identifier ()
  (parsec-and
   (jsp-whitespace)
   (parsec-re "[_[:alnum:]]+")))

(defun jsp-string-tq ()
  (parsec-and
   (jsp-whitespace)
   (parsec-query (parsec-re "\"\"\"\\(\\(?:.\\|\n\\)*?\\)\"\"\"") :group 1)))

(defun jsp-string-dq ()
  (parsec-and
   (jsp-whitespace)
   (parsec-query (parsec-re "\"\\(\\(?:.\\|\n\\)*?\\)\"") :group 1)))

(defun jsp-string ()
  (parsec-and
   (jsp-whitespace)
   (parsec-or (jsp-string-tq)
              (jsp-string-dq))))

(defun jsp-comment ()
  (parsec-and
   (jsp-whitespace)
   (parsec-re "#.*?$")))

(defun jsp-comment-multiline ()
  ;; use this as an example of the parsec-many parsec-try combination
  (parsec-many
   (parsec-try
    (parsec-and
     (jsp-whitespace)
     (parsec-re "#.*?$")))))

(defun jsp-end ()
  ;; FIXME: Deal with [1, 2, 3][end] expressions.
  (parsec-and
   (jsp-whitespace)
   (parsec-str "end")))

(defun jsp-other ()
  (parsec-and
   (jsp-whitespace)
   (parsec-many-till-as-string
    (parsec-any-ch)
    (parsec-lookahead
     (parsec-or (parsec-try (jsp-end))
                (parsec-try (jsp-comment))
                (parsec-try (jsp-string))
                ;;(jsp-block)
                (parsec-eof))))))

(defun jsp-expression ()
  (parsec-and
   (jsp-whitespace)
   (parsec-or (parsec-eof)
              (jsp-comment)
              (jsp-string)
              ;;(jsp-block)
              (jsp-other)
              )))

(defun jsp-module ()
  (parsec-collect
   (parsec-str "module")
   (jsp-identifier)))

(defun jsp-function ()
  (parsec-collect
   (parsec-str "function")
   (jsp-identifier)))

(defun jsp-block ()
  (parsec-and
   (jsp-whitespace)
   (parsec-collect*
    (parsec-or (jsp-module)
               (jsp-function))
    (parsec-many-till
     (parsec-try (jsp-expression))
     (parsec-lookahead (parsec-try (jsp-end))))
    (jsp-end))))


(provide 'julia-snail-parser)
