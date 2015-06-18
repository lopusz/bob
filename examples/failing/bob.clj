(bob-module)

(defrule
  [ [ (inp "./sleep_touch.sh") 3 (out "A") 1 (cpu 2) ]
    [ (inp "./sleep_touch.sh") 3 (out "B") 0 (cpu 3) ]
    [ (inp "./sleep_touch.sh") 6 (out "C") 0 ] 
    [ (inp "./sleep_touch.sh") 6 (out "D") 0 ] 
    [ (inp "./sleep_touch.sh") 6 (out "E") 0 ] ])
