(bob-module)

(defrule
   [ "grep -e '^....$'"
       (inp "/usr/share/dict/words") ">" (out "4.txt") ])

