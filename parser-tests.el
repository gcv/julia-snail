(require 'julia-snail-parser)
(require 'ert)


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
   (endp
    (parsec-with-input ""
      (jsp-expression))))
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
      (jsp-block)))))


(ert-deftest jsp-test-files ()
  (should
   (equal
    '(((:module 1 "Alpha")
       ("# comment"
        ((:function 24 "t1")
         ("(x)\n  x + 10\n  a = [1, 2, 3]\n  a[1:end]")
         (:end 75))
        "println(" "hi" ")")
       (:end 93))
      ((:module 98 "Bravo")
       (((:function 111 "t2")
         ("(y)")
         (:end 126)))
       (:end 130)))
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
end"
      (jsp-file)))))
