;;; tests/implicit-modules.el --- Julia Snail implicit modules test - full -*- lexical-binding: t -*-


;;; --- requirements

(require 'ert)

(require 'julia-snail)


;;; --- helpers

(defmacro js-with-julia-session (repl-buf &rest body)
  (declare (indent 1))
  `(let ((julia-snail-repl-buffer ,repl-buf))
     (unwind-protect
         ,@body
       (let ((kill-buffer-query-functions nil))
         (kill-buffer julia-snail-repl-buffer)))))


;;; --- tests

(ert-deftest js-test-implicit-modules-bare ()
  (let ((repl-buf (format "*julia* %s" (symbol-name (gensym))))
        (source-buf-1 (find-file (julia-snail-test-file-path "implicit-none.jl"))))
    (unwind-protect
        (js-with-julia-session repl-buf
          (julia-snail)
          (let ((process-buf (get-buffer (julia-snail--process-buffer-name repl-buf))))
            ;; look at the result of evaluating "implicit-none.jl"
            (with-current-buffer source-buf-1
              (julia-snail-test-send-buffer-file-sync))
            (let ((file-includes (gethash process-buf julia-snail--cache-proc-implicit-file-module)))
              (should (hash-table-empty-p file-includes)))))
      (kill-buffer source-buf-1))))

(ert-deftest js-test-implicit-modules-export-parse ()
  (let ((repl-buf (format "*julia* %s" (symbol-name (gensym))))
        (source-buf (find-file (julia-snail-test-file-path "implicit-export.jl"))))
    (unwind-protect
        (js-with-julia-session repl-buf
          (julia-snail)
          (with-current-buffer source-buf
            (let ((includes (julia-snail--cst-includes (get-buffer (current-buffer)))))
              (should
               (equal
                '("MyModule")
                (gethash (julia-snail-test-file-path "file1.jl") includes)))
              (should
               (equal
                '("MyModule")
                (gethash (julia-snail-test-file-path "file2.jl") includes))))))
      (kill-buffer source-buf))))

(ert-deftest js-test-implicit-modules-multiple ()
  (let ((repl-buf (format "*julia* %s" (symbol-name (gensym))))
        (source-buf-1 (find-file (julia-snail-test-file-path "implicit-multiple.jl")))
        (source-buf-2 (find-file (expand-file-name "b2.jl"))))
    (unwind-protect
        (js-with-julia-session repl-buf
          (julia-snail)
          (let ((process-buf (get-buffer (julia-snail--process-buffer-name repl-buf))))
            ;; look at the result of evaluating "implicit-multiple.jl"
            (with-current-buffer source-buf-1
              (julia-snail-test-send-buffer-file-sync))
            (let ((file-includes (gethash process-buf julia-snail--cache-proc-implicit-file-module)))
              (should
               (equal
                '("Alpha")
                (gethash (expand-file-name "a1.jl") file-includes)))
              (should
               (equal
                '("Alpha")
                (gethash (expand-file-name "a2.jl") file-includes)))
              (should
               (equal
                '("Bravo")
                (gethash (expand-file-name "b1.jl") file-includes)))
              (should
               (equal
                '("Bravo")
                (gethash (expand-file-name "b2.jl") file-includes))))
            ;; look at the result of diving into "b2.jl", which contains a nested
            ;; include() in a module
            (with-current-buffer source-buf-2
              (julia-snail-test-send-buffer-file-sync))
            (let ((file-includes (gethash process-buf julia-snail--cache-proc-implicit-file-module)))
              (should
               (equal
                '("Bravo" "Charlie")
                (gethash (expand-file-name "b3.jl") file-includes))))))
      (kill-buffer source-buf-2)
      (kill-buffer source-buf-1))))
