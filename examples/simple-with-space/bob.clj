(bob-module)

(defrule
   [ [  "echo \"Test of stderr...\" 1>&2;"
        "echo \"Test of stdout...\";"
        "echo " 1 2.0 ";"
        "cat" (inp "in1 with spaces.txt" "in2 with spaces.txt") ">" (out "out with spaces.txt") ]
      [ "cat " (inp "out with spaces.txt") ">" (out "out2 with spaces.txt") ] ])
