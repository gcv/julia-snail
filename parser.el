;;; parser.el --- Julia Snail parser -*- lexical-binding: t -*-


;;; --- requirements

(require 'dash)
(require 'parsec)


;;; --- parser rules

(defun julia-snail-parser-*whitespace ()
  (parsec-many-as-string
   (parsec-re "[[:space:]\r\n]")))

(defun julia-snail-parser-*identifier ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-re "[._[:alnum:]]+")))

(defmacro julia-snail-parser-*keyword (kw)
  `(parsec-return
       (parsec-str ,kw)
     (parsec-lookahead (parsec-re "[^[:alnum:]]"))))

(defun julia-snail-parser-*string-tq ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-query (parsec-re "\"\"\"\\(\\(?:.\\|\n\\)*?\\)\"\"\"") :group 1)))

(defun julia-snail-parser-*string-dq ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-query (parsec-re "\"\\(\\(?:.\\|\n\\)*?\\)\"") :group 1)))

(defun julia-snail-parser-*string ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-or (julia-snail-parser-*string-tq)
              (julia-snail-parser-*string-dq))))

(defun julia-snail-parser-*comment ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-re "#.*?$")))

(defun julia-snail-parser-*comment-multiline ()
  ;; use this as an example of the parsec-many parsec-try combination
  (parsec-many
   (parsec-try
    (parsec-and
     (julia-snail-parser-*whitespace)
     (parsec-re "#.*?$")))))

(defun julia-snail-parser-*end ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (julia-snail-parser-parsec-query (parsec-re "end") :end)))

(defun julia-snail-parser-*other ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-many-till-as-string
    (parsec-or
     (parsec-re "\\[.*?end.*?\\]") ; deal with Julia end syntax in brackets
     (parsec-any-ch))
    (parsec-lookahead
     (parsec-or
      (parsec-try (parsec-eof))
      (parsec-try (julia-snail-parser-*end))
      (parsec-try (julia-snail-parser-*comment))
      (parsec-try (julia-snail-parser-*string))
      (parsec-try (julia-snail-parser-*block)))))))

(defun julia-snail-parser-*expression ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-or (julia-snail-parser-*comment)
              (julia-snail-parser-*string)
              (julia-snail-parser-*block)
              (julia-snail-parser-*other))))

(defun julia-snail-parser-*start-module ()
  (-snoc
   (julia-snail-parser-parsec-query (parsec-or
             (julia-snail-parser-*keyword "module")
             (julia-snail-parser-*keyword "baremodule"))
            :module)
   (julia-snail-parser-*identifier)))

(defun julia-snail-parser-*start-function ()
  (-snoc
   (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "function") :function)
   (parsec-optional (julia-snail-parser-*identifier))))

(defun julia-snail-parser-*start-macro ()
  (-snoc
   (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "macro") :macro)
   (julia-snail-parser-*identifier)))

(defun julia-snail-parser-*start-type ()
  (-snoc
   (julia-snail-parser-parsec-query (parsec-or
             (julia-snail-parser-*keyword "abstract type")
             (julia-snail-parser-*keyword "primitive type"))
            :type)
   (julia-snail-parser-*identifier)))

(defun julia-snail-parser-*start-struct ()
  (-snoc
   (julia-snail-parser-parsec-query (parsec-or
             (julia-snail-parser-*keyword "struct")
             (julia-snail-parser-*keyword "mutable struct"))
            :struct)
   (julia-snail-parser-*identifier)))

(defun julia-snail-parser-*start-if ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "if") :if))

(defun julia-snail-parser-*start-while ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "while") :while))

(defun julia-snail-parser-*start-for ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "for") :for))

(defun julia-snail-parser-*start-begin ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "begin") :begin))

(defun julia-snail-parser-*start-quote ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "quote") :quote))

(defun julia-snail-parser-*start-try ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "try") :try))

(defun julia-snail-parser-*start-let ()
  (julia-snail-parser-parsec-query (julia-snail-parser-*keyword "let") :let))

(defun julia-snail-parser-*block ()
  (parsec-and
   (julia-snail-parser-*whitespace)
   (parsec-collect*
    (parsec-or (julia-snail-parser-*start-module)
               (julia-snail-parser-*start-function)
               (julia-snail-parser-*start-macro)
               (julia-snail-parser-*start-type)
               (julia-snail-parser-*start-struct)
               (julia-snail-parser-*start-if)
               (julia-snail-parser-*start-while)
               (julia-snail-parser-*start-for)
               (julia-snail-parser-*start-begin)
               (julia-snail-parser-*start-quote)
               (julia-snail-parser-*start-try)
               (julia-snail-parser-*start-let))
    (parsec-many-till
     (julia-snail-parser-*expression)
     (parsec-lookahead
      (parsec-or
       (parsec-eof)
       (parsec-try (julia-snail-parser-*end)))))
    (julia-snail-parser-*end))))

(defun julia-snail-parser-*file ()
  ;; XXX: This should be simply:
  ;; (parsec-many
  ;;  (julia-snail-parser-*expression))
  ;; but that causes infinte loops with stray "end"s at the end of the file. :(
  (parsec-many-till
   (julia-snail-parser-*expression)
   (parsec-lookahead
    (parsec-or
     (parsec-eof)
     (parsec-try (julia-snail-parser-*end))))))


;;; --- helpers

(defmacro julia-snail-parser-parsec-query (parser &optional placeholder)
  "Similar to parsec-query, but always returns the point position
at which the parser started matching. If placeholder is given,
replace the result of the parser with it."
  (let ((start (gensym))
        (res (gensym))
        (ph (gensym)))
    `(let ((,start (point))
           (,res ,parser)
           (,ph ,placeholder))
       (list (if ,ph ,ph ,res)
             ,start))))


;;; --- parse tree processing functions

(defun julia-snail-parser-parse (buf)
  (save-excursion
    (with-current-buffer buf
      (goto-char (point-min))
      (parsec-parse (julia-snail-parser-*file)))))

(defun julia-snail-parser-blocks (tree)
  (cond
   ((null tree)
    tree)
   ((atom tree)
    nil)
   ((and (listp tree)
         (listp (-first-item tree))
         (keywordp (-first-item (-first-item tree))))
    (let* ((block tree)
           (head (-first-item block))
           (body (-second-item block))
           (tail (-last-item block)))
      (-remove #'null (list (-first-item head)
                            (-second-item head)
                            (+ 3 (-second-item tail)) ;; end location
                            (when (-third-item head) (substring-no-properties (-third-item head)))
                            (unless (equal body tail)
                              (-remove #'null (-map #'julia-snail-parser-blocks body)))))))
   (t ; list
    (-remove #'null (cons (julia-snail-parser-blocks (car tree))
                          (julia-snail-parser-blocks (cdr tree)))))))

(defun julia-snail-parser-block-path (blocks pt)
  (cl-labels ((helper (node)
                      (if (keywordp (-first-item node))
                          (when (and (>= pt (-second-item node))
                                     (<= pt (-third-item node)))
                            (if (atom (-last-item node))
                                (list (-first-item node)
                                      (-second-item node)
                                      (-third-item node)
                                      (if (stringp (-fourth-item node))
                                          (-fourth-item node)
                                        :nil))
                              (let ((child-check (helper (-last-item node))))
                                (when child-check
                                  (list (-first-item node)
                                        (-second-item node)
                                        (-third-item node)
                                        (if (stringp (-fourth-item node))
                                            (-fourth-item node)
                                          :nil)
                                        child-check)))))
                        (-first-item (-remove #'null (-map #'helper node))))))
    (-partition
     4
     (-map (lambda (x) (if (equal :nil x) nil x))
           (-> (-remove #'null (-map #'helper blocks))
               -first-item
               -flatten)))))


;;; --- queries

;; needed queries:
;; - current module (including nesting)
;; - current top-level block (top-level: either a direct child of a module, or actually top level of block list)

(defun julia-snail-parser-query-current-module (block-path)
  ;; Implementation: Remove everything from the list which is not a module, and
  ;; return the resulting module names. Fall back to Main if nothing comes back.
  )

(defun julia-snail-parser-query-current-top-level-block (block-path)
  )


;;; --- entry point

(defun julia-snail-parser-query (buffer query)
  ;; FIXME: This needs to do the parsing, and must error-check and
  ;; error-report properly.
  ;; FIXME: Maybe change /everything/ other than this function to
  ;; julia-snail-parser-- namespace?
  (cond ((equal :module query)
         ;; ...
         ))
  )


;;; --- done

(provide 'julia-snail-parser)
