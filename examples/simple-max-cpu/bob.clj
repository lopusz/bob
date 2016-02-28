(bob-module)

(defrule
   [ [  "echo \"Test of stderr...\" 1>&2;"
        "echo \"Test of stdout...\";"
        "echo " 1 2.0 ";"
        "cat" (inp "in1.txt" "in2.txt") ">" (out "out.txt") (cpu* (get-max-cpu))]
      [ "cat " (inp "out.txt") ">" (out "out2.txt") ] ])
