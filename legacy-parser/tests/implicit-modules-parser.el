;;; tests/implicit-modules-parser.el --- Julia Snail implicit module tests - parser level -*- lexical-binding: t -*-


;;; --- requirements

(require 'ert)

(require 'julia-snail)
(require 'julia-snail-parser)


;;; --- tests

(ert-deftest js-test-mapping ()
  (with-temp-buffer
    (insert-file (julia-snail-test-file-path "implicit-multiple.jl"))
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
    (insert-file (julia-snail-test-file-path "implicit-nested.jl"))
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
