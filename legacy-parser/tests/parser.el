;;; tests/parser.el --- Julia Snail parser tests -*- lexical-binding: t -*-


;;; --- requirements

(require 'ert)

(require 'julia-snail)
(require 'julia-snail-parser)


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
      (julia-snail-parser--*string))))
  (should
   (equal
    "one\\\"two"
    (parsec-with-input "\"one\\\"two\""
      (julia-snail-parser--*string))))
  ;; non-standard literals
  (should
   (equal
    "one"
    (parsec-with-input "r\"one\"i"
      (julia-snail-parser--*string))))
  (should
   (equal
    "one"
    (parsec-with-input "raw\"one\""
      (julia-snail-parser--*string))))
  (should
   (equal
    '(((:function 1 "f")
       ("(name)" "m = " "two" "components = match" "(r\"(.*?).(.*)\", name)")
       (:end 74))
      "one" "")
    (parsec-with-input "function f(name)
   m = \"two\"
   components = match(r\"(.*?)\.(.*)\", name)
end
\"one\"
"
      (julia-snail-parser--*file)))))

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

(ert-deftest jsp-test-end ()
  (should
   (equal
    '(:end 1)
    (parsec-with-input "end"
      (julia-snail-parser--*end))))
  (should
   (equal
    '(:end 2)
    (parsec-with-input " end"
      (julia-snail-parser--*end))))
  (should
   (equal
    '(:end 2)
    (parsec-with-input ";end"
      (julia-snail-parser--*end))))
  (should
   (equal
    '(:end 2)
    (parsec-with-input ";end"
      (julia-snail-parser--*end))))
  (should
   (parsec-error-p
    (parsec-with-input "append"
      (julia-snail-parser--*end))))
  (should
   (parsec-error-p
    (parsec-with-input "# end"
      (julia-snail-parser--*end)))))

(ert-deftest jsp-test-other ()
  (should
   (equal
    "one "
    (parsec-with-input "one function(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (parsec-error-p
    (parsec-with-input "function(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (parsec-error-p
    (parsec-with-input "   function(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (equal
    "prefunction"
    (parsec-with-input "prefunction(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (equal
    "pre_function"
    (parsec-with-input "pre_function(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (equal
    "function_post"
    (parsec-with-input "function_post(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (equal
    "functionpost"
    (parsec-with-input "functionpost(x) x; end"
      (julia-snail-parser--*other))))
  (should
   (equal
    "1:"
    (parsec-with-input "1:((function(); 1; end)())"
      (julia-snail-parser--*other))))
  (should
   (parsec-error-p
    (parsec-with-input "(function(); 1; end)()*(function(); 2; end)()"
      (julia-snail-parser--*other))))
  (should
   (parsec-error-p
    (parsec-with-input "end"
      (julia-snail-parser--*other))))
  ;; "[end]" checks have to use julia-snail-parser--*block
  (should
   (equal
    '((:module 1 "Alpha")
      ("A = B" "[1:end]")
      (:end 27))
    (parsec-with-input "module Alpha
A = B[1:end]
end"
      (julia-snail-parser--*block))))
  (should
   (equal
    '((:module 1 "Alpha")
      ("A = B" "[ 1 : end ]")
      (:end 31))
    (parsec-with-input "module Alpha
A = B[ 1 : end ]
end"
      (julia-snail-parser--*block)))))

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
   (parsec-error-p
    (parsec-with-input "end"
      (julia-snail-parser--*expression))))
  (should
   (equal
    '("alpha "
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
    "alpha\n"
    (parsec-with-input "
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
       "delta + echo\nfoxtrot = golf"
       "()"
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
        ("(x)" "x + 10\n  a = " "[1, 2, 3]" "a" "[1:end]")
        (:end 77))
       "println" "(\"hi\")")
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
        "echo\n"
        ((:function 29 "t1")
         ("(x)" "x + 10\n  a = " "[1, 2, 3]" "a" "[1:end]")
         (:end 80))
        "println" "(\"hi\")")
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
   (null
    (parsec-with-input "end"
      (julia-snail-parser--*file))))
  ;; bracket things
  (should
   (equal
    '(((:module 1 "Alpha")
       (((:function 14 "f")
         ("()"
          ((:for 30)
           ("in A" "[2:end]")
           (:end 49)))
         (:end 53))
        "stuff = " "[]")
       (:end 68)))
    (parsec-with-input "module Alpha
function f()
   for in A[2:end]
   end
end
stuff = []
end"
      (julia-snail-parser--*file))))
  (should
   (equal
    '(((:module 1 "A")
       (((:function 11 "f")
         ("(x)" "one = " "[]")
         (:end 37))
        "two = " "[]")
       (:end 52))
      "")
    (parsec-with-input "module A

function f(x)
   one = []
end

two = []

end
"
      (julia-snail-parser--*file))))
  ;; nested code with case-sensitive Module check
  (should
   (equal
    '((:function 1 "xref_backend_identifiers")
      ("(ns)" "check = "
       ((:function 50 nil)
        ("(pn)"
         ((:try 69)
          ("typeof" "(eval(pn))" "!= " "Module\n      catch\n         true\n      ")
          (:end 141)))
        (:end 148))
       "filter" "(check, propertynames(Main, true))")
      (:end 196))
    (parsec-with-input "function xref_backend_identifiers(ns)
   check = function(pn)
      try
         typeof(eval(pn)) != Module
      catch
         true
      end
   end
   filter(check, propertynames(Main, true))
end"
      (julia-snail-parser--*block)))))

(ert-deftest jsp-test-all-blocks ()
  (should
   (equal
    '(((:struct 2 "Point")
       ("x\n  y\n")
       (:end 23))
      ((:macro 28 "m1")
       ("(x)" "x\n")
       (:end 44))
      ((:struct 49 "Vector")
       ("x\n  y\n  z\n")
       (:end 75))
      ((:function 80 "t1")
       ("(x)"
        ((:try 97)
         (((:if 105)
           ("alpha\n      i = 0\n      "
            ((:while 132)
             ("i < 10\n        "
              ((:for 153)
               ("x in 1:3\n          println" "(x * i)")
               (:end 199)))
             (:end 209))
            "ex = "
            ((:quote 224)
             ("x + 3\n      ")
             (:end 250)))
           (:end 258))
          "catch\n    z = 10\n    "
          ((:begin 285)
           (((:let 297)
             ("w = z * 4\n        println" "(w)")
             (:end 336)))
           (:end 344))
          "finally\n    stuff" "()")
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
    '(("("
       ((:function 2 nil)
        ("(x)" "; return 2x; ")
        (:end 26))
       ")")
      "(3)")
    (parsec-with-input "(function(x); return 2x; end)(3)"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-embedded-end ()
  (should
   (equal
    '((:function 1 "f1")
      ("()" "C = @Persistent " "[1 2 3]" "append" "(C, 4)")
      (:end 58))
    (parsec-with-input "function f1()
   C = @Persistent [1 2 3]
   append(C, 4)
end"
      (julia-snail-parser--*block))))
  (equal
   '(((:function 1 "f")
      ("()"
       ((:for 17)
        ("a in A" "[end]")
        (:end 36)))
      (:end 40))
     ((:function 45 "g")
      ("()" "A" "[end:end]")
      (:end 72))
     "")
   (parsec-with-input "function f()
   for a in A[end]
   end
end

function g()
   A[end:end]
end
"
     (julia-snail-parser--*file))))

(ert-deftest jsp-test-whole-file-blocks ()
  (let ((blocks
         (with-temp-buffer
           (insert-file (julia-snail-test-file-path "blocks.jl"))
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
      '((:module 1 549 "Alpha"))
      (julia-snail-parser--block-path blocks 1)))
    (should
     (equal
      '((:module 1 549 "Alpha"))
      (julia-snail-parser--block-path blocks 2)))
    (should
     (equal
      '((:module 1 549 "Alpha"))
      (julia-snail-parser--block-path blocks 10)))
    (should
     (equal
      '((:module 1 549 "Alpha")
        (:module 15 544 "Bravo"))
      (julia-snail-parser--block-path blocks 15)))
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
      (list :module nil
            :block '(:function 50 80 "f1"))
      (julia-snail-parser--query-top-level-block block-path)))))

(ert-deftest jsp-test-query-fail-to-parse ()
  (with-temp-buffer
    (insert-file (julia-snail-test-file-path "bad-syntax.jl"))
    (should-error
     (julia-snail-parser-query (current-buffer) 1 :module))))

(ert-deftest jsp-test-query-file-module ()
  (with-temp-buffer
    (insert-file (julia-snail-test-file-path "blocks.jl"))
    (should
     (equal
      '("Alpha")
      (julia-snail-parser-query (current-buffer) 1 :module)))
    (should
     (equal
      '("Alpha")
      (julia-snail-parser-query (current-buffer) 2 :module)))
    (should
     (equal
      '("Alpha" "Bravo")
      (julia-snail-parser-query (current-buffer) 55 :module)))
    (should
     (null
      (julia-snail-parser-query (current-buffer) 550 :module)))
    (should
     (equal
      '("Charlie")
      (julia-snail-parser-query (current-buffer) 551 :module)))
    (should
     (equal
      '("Delta")
      (julia-snail-parser-query (current-buffer) 585 :module)))
    (should
     (null
      (julia-snail-parser-query (current-buffer) 787 :module)))))

(ert-deftest jsp-test-query-file-top-level-block ()
  (with-temp-buffer
    (insert-file (julia-snail-test-file-path "blocks.jl"))
    (should-error
     (julia-snail-parser-query (current-buffer) 1 :top-level-block))
    (should
     (equal
      (list :module '("Alpha" "Bravo")
            :block '(:macro 29 65 "m1"))
      (julia-snail-parser-query (current-buffer) 55 :top-level-block)))
    (should
     (equal
      (list :module '("Alpha" "Bravo")
            :block '(:struct 67 85 "s1"))
      (julia-snail-parser-query (current-buffer) 77 :top-level-block)))
    (should
     (equal
      (list :module '("Alpha" "Bravo")
            :block '(:function 165 281 "t1"))
      (julia-snail-parser-query (current-buffer) 260 :top-level-block)))
    (should
     (equal
      (list :module '("Delta" "Echo")
            :block '(:let 712 740 nil))
      (julia-snail-parser-query (current-buffer) 723 :top-level-block)))
    (should
     (equal
      (list :module '("Delta" "Echo")
            :block '(:function 742 776 "t5"))
      (julia-snail-parser-query (current-buffer) 756 :top-level-block)))
    (should-error
     (julia-snail-parser-query (current-buffer) 787 :top-level-block))))

(ert-deftest jsp-test-embedded-keywords ()
  (should
   (equal
    '(((:module 1 "Alpha")
       ("\ndocstring\n"
        ((:function 32 "test_fn")
         ("(module_name)" "hi")
         (:end 70)))
       (:end 74))
      "")
    (parsec-with-input "module Alpha
\"\"\"
docstring
\"\"\"
function test_fn(module_name)
   \"hi\"
end
end
"
      (julia-snail-parser--*file))))
  (should
   (equal
    '(((:module 1 "Alpha")
       (((:function 14 "f")
         ("()" "modules = " "[\"One\", \"Two\"]")
         (:end 55)))
       (:end 59)))
    (parsec-with-input "module Alpha
function f()
   modules = [\"One\", \"Two\"]
end
end"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-brackets ()
  (should
   (equal
    "[2x * B]"
    (parsec-with-input
        "[2x * B]"
      (julia-snail-parser--*brackets))))
  (should
   (equal
    "[2x * B[end] for x in A[2:end]]"
    (parsec-with-input
        "[2x * B[end] for x in A[2:end]]"
      (julia-snail-parser--*expression)))))

(ert-deftest jsp-test-list-comprehension ()
  (should
   (equal
    '(((:function 1 "f")
       ("(ex::Expr)"
        "kvtuples = " "[:($(esc(kv.args[end-1])), $(esc(kv.args[end])))\n                for kv in ex.args[2:end]]")
       (:end 128)))
    (parsec-with-input "function f(ex::Expr)
    kvtuples = [:($(esc(kv.args[end-1])), $(esc(kv.args[end])))
                for kv in ex.args[2:end]]
end"
      (julia-snail-parser--*file))))
  (should
   (equal
    '(((:function 1 "f")
       ("(A, B)" "[2x * B[end]\n    for x in A[2:end]]")
       (:end 57)))
    (parsec-with-input "function f(A, B)
   [2x * B[end]
    for x in A[2:end]]
end"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-parenthesized-expressions ()
  (should
   (equal
    '(("("
       ((:function 2 nil)
        ("(x)" "; 2x; ")
        (:end 19))
       ")")
      "(10)")
    (parsec-with-input "(function(x); 2x; end)(10)"
      (julia-snail-parser--*file))))
  ;; now introduce an unterminated for:
  (should
   (equal
    '("f" "(x)" "= " "(+(g()...) for _ in 1:n)")
    (parsec-with-input "f(x) = (+(g()...) for _ in 1:n)"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-include-expressions ()
  (should
   (equal
    '(((:module 1 "Alpha")
       ((:include 14 "a1.jl")
        ((:function 31 "f1")
         ("()" "return value")
         (:end 63))
        (:include 67 "a2.jl"))
       (:end 84)))
    (parsec-with-input "module Alpha
include(\"a1.jl\")
function f1()
   \"return value\"
end
include(\"a2.jl\")
end"
      (julia-snail-parser--*file))))
  ;; embedded in a comment
  (should
   (equal
    '(((:module 1 "Alpha")
       ("# include(\"a1.jl\")"
        ((:function 33 "f1")
         ("()" "return value")
         (:end 65))
        (:include 69 "a2.jl"))
       (:end 86)))
    (parsec-with-input "module Alpha
# include(\"a1.jl\")
function f1()
   \"return value\"
end
include(\"a2.jl\")
end"
      (julia-snail-parser--*file)))))

(ert-deftest jsp-test-query-includes ()
  ;; just one module with includes
  (should
   (equal
    '((:module "Alpha"
               ((:include "a1.jl")
                (:include "a2.jl"))))
    (julia-snail-parser--includes
     (parsec-with-input "module Alpha
include(\"a1.jl\")
function f1()
   \"return value\"
end
include(\"a2.jl\")
end"
       (julia-snail-parser--*file)))))
  ;; separate modules in one file
  (should
   (equal
    '((:module "Alpha"
               ((:include "a1.jl")
                (:include "a2.jl")))
      (:module "Bravo"
               ((:include "b1.jl")
                (:include "b2.jl"))))
    (julia-snail-parser--includes
     (parsec-with-input "module Alpha
include(\"a1.jl\")
function f1()
   \"return value\"
end
include(\"a2.jl\")
end
module Bravo
include(\"b1.jl\")
include(\"b2.jl\")
end"
       (julia-snail-parser--*file)))))
  ;; nested modules
  (should
   (equal
    '((:module "Alpha"
               ((:include "a1.jl")
                (:include "a2.jl")
                (:module "Bravo"
                         ((:include "b1.jl")
                          (:include "b2.jl"))))))
    (julia-snail-parser--includes
     (parsec-with-input "module Alpha
include(\"a1.jl\")
function f1()
   \"return value\"
end
include(\"a2.jl\")
module Bravo
include(\"b1.jl\")
include(\"b2.jl\")
end
end"
       (julia-snail-parser--*file))))))

(ert-deftest jsp-test-anonymous-functions ()
  (should
   (equal
    '("f = "
      ((:function 5 nil)
       ("()" "42\n")
       (:end 22)))
    (parsec-with-input "f = function ()
  42
end"
      (julia-snail-parser--*file))))
  (should
   (equal
    '(((:struct 1 "sample")
       ("a\n")
       (:end 19))
      ((:function 23 nil)
       ("(x::sample)" "(b)" "x.a + b\n")
       (:end 57)))
    (parsec-with-input "struct sample
  a
end
function (x::sample)(b)
  x.a + b
end"
      (julia-snail-parser--*file)))))
