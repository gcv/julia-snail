;;; julia-snail/tests/parser.el --- Julia Snail parser tests -*- lexical-binding: t -*-


;;; --- requirements

(require 'ert)

(require 'julia-snail-parser "../parser.el")


;;; --- tests

(ert-deftest jsp-test-strings ()
  (should
   (equal
    "one"
    (parsec-with-input "
\"one\"
\"two\"
"
      (julia-snail-parser--*string))))
  (should
   (equal
    "one\ntwo\nthree"
    (parsec-with-input "
\"one
two
three\"
"
      (julia-snail-parser--*string))))
  (should
   (equal
    "one\ntwo\nthree"
    (parsec-with-input "
\"\"\"one
two
three\"\"\"
"
      (julia-snail-parser--*string)))))

(ert-deftest jsp-test-comments ()
  (should
   (equal
    "# one"
    (parsec-with-input "
# one
# two
"
      (julia-snail-parser--*comment))))
  (should
   (equal
    '("# one" "# two" "## three")
    (parsec-with-input "
# one
# two
   ## three
"
      (julia-snail-parser--*comment-multiline)))))

(ert-deftest jsp-test-expressions ()
  (should
   (equal
    "one"
    (parsec-with-input "\"one\""
      (julia-snail-parser--*expression))))
  (should
   (equal
    "# comment"
    (parsec-with-input "# comment"
      (julia-snail-parser--*expression))))
  (should
   (equal
    "alpha"
    (parsec-with-input "alpha"
      (julia-snail-parser--*expression))))
  (should
   (equal
    ""
    (parsec-with-input "end"
      (julia-snail-parser--*expression))))
  (should
   (equal
    '("alpha"
      (:end 7))
    (parsec-with-input "alpha end"
      (parsec-collect (julia-snail-parser--*expression)
                      (julia-snail-parser--*end)))))
  (should
   (equal
    "alpha"
    (parsec-with-input "
\"alpha\"
\"bravo\"
# charlie
delta
"
      (julia-snail-parser--*expression))))
  (should
   (equal
    "# comment"
    (parsec-with-input "
# comment
\"alpha\"
bravo
"
      (julia-snail-parser--*expression))))
  (should
   (equal
    '("# comment" "alpha" "bravo\n")
    (parsec-with-input "
# comment
\"alpha\"
bravo
"
      (parsec-many
       (parsec-try
        (julia-snail-parser--*expression))))))
  (should
   (equal
    "alpha"
    (parsec-with-input "
alpha
\"bravo\"
\"charlie\"
# delta
echo
"
      (julia-snail-parser--*expression))))
  (should
   (equal
    ""
    (parsec-with-input "
end
alpha
\"bravo\"
\"charlie\"
# delta
echo
"
      (julia-snail-parser--*expression)))))

(ert-deftest jsp-test-blocks ()
  (should
   (equal
    '((:module 1 "Alpha")
      ("# bravo"
       "charlie"
       "delta + echo\nfoxtrot = golf()"
       "# hotel")
      (:end 70))
    (parsec-with-input "module Alpha
# bravo
\"charlie\"
delta + echo
foxtrot = golf()
# hotel
end
"
      (julia-snail-parser--*block))))
  (should
   (equal
    '((:module 1 "Alpha")
      ("# comment"
       ((:function 26 "t1")
        ("(x)\n  x + 10\n  a = [1, 2, 3]\n  a[1:end]")
        (:end 77))
       "println(" "hi" ")")
      (:end 97))
    (parsec-with-input "module Alpha

# comment

function t1(x)
  x + 10
  a = [1, 2, 3]
  a[1:end]
end

println(\"hi\")

end
"
      (julia-snail-parser--*block))))
  ;; unterminated blocks
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha"
      (julia-snail-parser--*block))))
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha
alpha"
      (julia-snail-parser--*block))))
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha
alpha
function t1()
end"
      (julia-snail-parser--*block)))))

(ert-deftest jsp-test-files ()
  (should
   (equal
    '(((:module 1 "Alpha")
       ("# comment"
        "echo"
        ((:function 29 "t1")
         ("(x)\n  x + 10\n  a = [1, 2, 3]\n  a[1:end]")
         (:end 80))
        "println(" "hi" ")")
       (:end 98))
      ((:module 103 "Bravo")
       (((:function 116 "t2")
         ("(y)")
         (:end 131)))
       (:end 135))
      "")
    (parsec-with-input "module Alpha
# comment
echo
function t1(x)
  x + 10
  a = [1, 2, 3]
  a[1:end]
end
println(\"hi\")
end

module Bravo
function t2(y)
end
end
"
      (julia-snail-parser--*file))))
  ;; missing terminating "end"
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha
# comment
function t1(x)
  x + 10
  a = [1, 2, 3]
  a[1:end]
end
println(\"hi\")
end

module Bravo
function t2(y)
end
"
      (julia-snail-parser--*file))))
  ;; too many "end"s
  (should
   (equal
    '(((:module 1 "Alpha")
       (:end 14)))
    (parsec-with-input "module Alpha
end
end"
      (julia-snail-parser--*file))))
  (should
   (endp
    (parsec-with-input "end"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-all-blocks ()
  (should
   (equal
    '(((:struct 2 "Point")
       ("x\n  y")
       (:end 23))
      ((:macro 28 "m1")
       ("(x)\n  x")
       (:end 44))
      ((:struct 49 "Vector")
       ("x\n  y\n  z")
       (:end 75))
      ((:function 80 "t1")
       ("(x)"
        ((:try 97)
         (((:if 105)
           ("alpha\n      i = 0"
            ((:while 132)
             ("i < 10"
              ((:for 153)
               ("x in 1:3\n          println(x * i)")
               (:end 199)))
             (:end 209))
            "ex ="
            ((:quote 224)
             ("x + 3")
             (:end 250)))
           (:end 258))
          "catch\n    z = 10"
          ((:begin 285)
           (((:let 297)
             ("w = z * 4\n        println(w)")
             (:end 336)))
           (:end 344))
          "finally\n    stuff()")
         (:end 372)))
       (:end 376)))
    (parsec-with-input "
struct Point
  x
  y
end

macro m1(x)
  x
end

struct Vector
  x
  y
  z
end

function t1(x)
  try
    if alpha
      i = 0
      while i < 10
        for x in 1:3
          println(x * i)
        end
      end
      ex = quote
        x + 3
      end
    end
  catch
    z = 10
    begin
      let w = z * 4
        println(w)
      end
    end
  finally
    stuff()
  end
end"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-anonymous-functions ()
  (should
   (equal
    '(:function 1 nil)
    (parsec-with-input "function(x)"
      (julia-snail-parser--*start-function))))
  (should
   (equal
    '("("
      ((:function 2 nil)
       ("(x); return 2x;")
       (:end 26))
      ")(3)")
    (parsec-with-input "(function(x); return 2x; end)(3)"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-whole-file-blocks ()
  (let ((blocks
         (with-temp-buffer
           (insert-file
            ;; XXX: Obnoxious Elisp path construction for "files/blocks.jl".
            (concat
             (file-name-as-directory
              (concat (or (file-name-as-directory default-directory)
                          (file-name-directory load-file-name)) "files"))
             "blocks.jl"))
           (-> (current-buffer)
               julia-snail-parser--parse
               julia-snail-parser--blocks)))
        (expected-blocks
         '((:module 1 549 "Alpha"
                    ((:module 15 544 "Bravo"
                              ((:macro 29 65 "m1"
                                       ((:while 43 61)))
                               (:struct 67 85 "s1")
                               (:type 87 122 "NewReal")
                               (:type 124 163 "Special")
                               (:function 165 281 "t1"
                                          ((:for 208 277
                                                 ((:if 228 270)))))
                               (:function 283 439 "t2"
                                          ((:let 300 435
                                                 ((:try 325 428)))))
                               (:function 441 539 "t3"
                                          ((:begin 468 535
                                                   ((:quote 497 528)))))))))
           (:module 551 569 "Charlie")
           (:module 571 786 "Delta"
                    ((:begin 593 649
                             ((:if 602 645)))
                     (:function 651 697 "t4")
                     (:module 699 781 "Echo"
                              ((:let 712 740)
                               (:function 742 776 "t5"))))))))
    (should (equal expected-blocks blocks))
    (should
     (equal
      '((:module 1 549 "Alpha")
        (:module 15 544 "Bravo")
        (:macro 29 65 "m1")
        (:while 43 61 nil))
      (julia-snail-parser--block-path blocks 50)))
    (should
     (equal
      '((:module 1 549 "Alpha")
        (:module 15 544 "Bravo")
        (:type 124 163 "Special"))
      (julia-snail-parser--block-path blocks 130)))
    (should
     (equal
      '((:module 1 549 "Alpha")
        (:module 15 544 "Bravo")
        (:function 441 539 "t3")
        (:begin 468 535 nil)
        (:quote 497 528 nil))
      (julia-snail-parser--block-path blocks 500)))
    (should
     (equal
      '((:module 571 786 "Delta")
        (:function 651 697 "t4"))
      (julia-snail-parser--block-path blocks 660)))
    (should
     (equal
      '((:module 571 786 "Delta")
        (:module 699 781 "Echo")
        (:function 742 776 "t5"))
      (julia-snail-parser--block-path blocks 750)))))

(ert-deftest jsp-test-query-top-level-blocks ()
  (let ((block-path '((:module 1 549 "Alpha")
                      (:module 15 544 "Bravo")
                      (:function 165 281 "f1")
                      (:for 208 277 nil)
                      (:if 228 270 nil))))
    (should
     (equal
      (list :module '("Alpha" "Bravo")
            :block '(:function 165 281 "f1"))
      (julia-snail-parser--query-top-level-block block-path))))
  ;; more complicated nesting 1
  (let ((block-path '((:module 1 130 "Alpha")
                      (:module 10 120 "Bravo")
                      (:let 20 110 nil)
                      (:begin 30 100 nil)
                      (:function 50 80 "f1")
                      (:if 60 70 nil))))
    (should
     (equal
      (list :module '("Alpha" "Bravo")
            :block '(:let 20 110 nil))
      (julia-snail-parser--query-top-level-block block-path))))
  ;; more complicated nesting 2
  (let ((block-path '((:module 1 130 "Alpha")
                      (:begin 3 65 nil)
                      (:module 10 120 "Bravo")
                      (:let 20 110 nil)
                      (:begin 30 100 nil)
                      (:module 40 90 "Charlie")
                      (:function 50 80 "f1")
                      (:if 60 70 nil))))
    (should
     (equal
      (list :module '("Alpha" "Bravo" "Charlie")
            :block '(:function 50 80 "f1"))
      (julia-snail-parser--query-top-level-block block-path))))
  ;; no top-level module
  (let ((block-path '((:function 50 80 "f1")
                      (:if 60 70 nil))))
    (should
     (equal
      (list :module '("Main")
            :block '(:function 50 80 "f1"))
      (julia-snail-parser--query-top-level-block block-path)))))
