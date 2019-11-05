(require 'julia-snail-parser)
(require 'ert)


(ert-deftest jsp--test-strings ()
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


(ert-deftest jsp--test-comments ()
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


(ert-deftest jsp--expressions ()
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
    '("alpha" "end")
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


(ert-deftest jsp--blocks ()
  (should
   (equal
    '(("module" "Alpha")
      ("# bravo"
       "charlie"
       "delta + echo\nfoxtrot = golf()"
       "# hotel")
      "end")
    (parsec-with-input "module Alpha
# bravo
\"charlie\"
delta + echo
foxtrot = golf()
# hotel
end
"
      (jsp-block)))))
