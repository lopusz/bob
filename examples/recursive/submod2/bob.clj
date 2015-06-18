(bob-module)

(defrule
  [ [  "grep -e '^...$'" (inp "/usr/share/dict/words") ">" (out "3.txt")   (tag "a")]
    [  "grep -e '^.....$'" (inp "/usr/share/dict/words") ">" (out "5.txt") (tag "b")] ])
