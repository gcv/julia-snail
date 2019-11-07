(require 'ert)

(require 'julia-snail-parser "parser.el")


(ert-deftest jsp-test-strings ()
  (should
   (equal
    "one"
    (parsec-with-input "
\"one\"
\"two\"
"
      (jsp-string))))
  (should
   (equal
    "one\ntwo\nthree"
    (parsec-with-input "
\"one
two
three\"
"
      (jsp-string))))
  (should
   (equal
    "one\ntwo\nthree"
    (parsec-with-input "
\"\"\"one
two
three\"\"\"
"
      (jsp-string)))))


(ert-deftest jsp-test-comments ()
  (should
   (equal
    "# one"
    (parsec-with-input "
# one
# two
"
      (jsp-comment))))
  (should
   (equal
    '("# one" "# two" "## three")
    (parsec-with-input "
# one
# two
   ## three
"
      (jsp-comment-multiline)))))


(ert-deftest jsp-test-expressions ()
  (should
   (equal
    "one"
    (parsec-with-input "\"one\""
      (jsp-expression))))
  (should
   (equal
    "# comment"
    (parsec-with-input "# comment"
      (jsp-expression))))
  (should
   (equal
    "alpha"
    (parsec-with-input "alpha"
      (jsp-expression))))
  (should
   (equal
    ""
    (parsec-with-input "end"
      (jsp-expression))))
  (should
   (equal
    '("alpha"
      (:end 7))
    (parsec-with-input "alpha end"
      (parsec-collect (jsp-expression)
                      (jsp-end)))))
  (should
   (equal
    "alpha"
    (parsec-with-input "
\"alpha\"
\"bravo\"
# charlie
delta
"
      (jsp-expression))))
  (should
   (equal
    "# comment"
    (parsec-with-input "
# comment
\"alpha\"
bravo
"
      (jsp-expression))))
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
        (jsp-expression))))))
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
      (jsp-expression))))
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
      (jsp-expression)))))


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
      (jsp-block))))
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
      (jsp-block))))
  ;; unterminated blocks
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha"
      (jsp-block))))
  (should
   (parsec-error-p
    (parsec-with-input "module Alpha
alpha"
      (jsp-block))))
    (should
   (parsec-error-p
    (parsec-with-input "module Alpha
alpha
function t1()
end"
      (jsp-block)))))


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
      (jsp-file))))
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
      (jsp-file))))
  ;; too many "end"s
  (should
   (equal
    '(((:module 1 "Alpha")
       (:end 14)))
    (parsec-with-input "module Alpha
end
end"
      (jsp-file))))
  (should
   (endp
    (parsec-with-input "end"
      (jsp-file)))))


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
      (jsp-file)))))
