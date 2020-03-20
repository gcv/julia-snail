;;; julia-snail/tests/implicit-modules.el --- Julia Snail implicit module tests -*- lexical-binding: t -*-


;;; --- requirements

(require 'ert)

(require 'julia-snail)
(require 'julia-snail-parser)


;;; --- variables

(defvar js-test-file-implicit-multiple.jl
  ;; XXX: Obnoxious Elisp path construction for "files/implicit-multiple.jl".
  (concat
   (file-name-as-directory
    (concat (if load-file-name
                (file-name-directory load-file-name)
              (file-name-as-directory default-directory))
            "files"))
   "implicit-multiple.jl"))

(defvar js-test-file-implicit-nested.jl
  ;; XXX: Obnoxious Elisp path construction for "files/implicit-nested.jl".
  (concat
   (file-name-as-directory
    (concat (if load-file-name
                (file-name-directory load-file-name)
              (file-name-as-directory default-directory))
            "files"))
   "implicit-nested.jl"))


;;; --- tests

(ert-deftest js-test-mapping ()
  (with-temp-buffer
    (insert-file js-test-file-implicit-multiple.jl)
    (let* ((includes (julia-snail-parser-includes (current-buffer)))
           (mapping (julia-snail--module-make-implicit-module-map includes)))
      (should
       (equal
        '("Alpha")
        (gethash "a1.jl" mapping)))
      (should
       (equal
        '("Alpha")
        (gethash "a2.jl" mapping)))
      (should
       (equal
        '("Bravo")
        (gethash "b1.jl" mapping)))
      (should
       (equal
        '("Bravo")
        (gethash "b2.jl" mapping)))))
  (with-temp-buffer
    (insert-file js-test-file-implicit-nested.jl)
    (let* ((includes (julia-snail-parser-includes (current-buffer)))
           (mapping (julia-snail--module-make-implicit-module-map includes)))
      (should
       (equal
        '("Alpha")
        (gethash "a1.jl" mapping)))
      (should
       (equal
        '("Alpha")
        (gethash "a2.jl" mapping)))
      (should
       (equal
        '("Alpha" "Bravo")
        (gethash "b1.jl" mapping)))
      (should
       (equal
        '("Alpha" "Bravo")
        (gethash "b2.jl" mapping))))))
