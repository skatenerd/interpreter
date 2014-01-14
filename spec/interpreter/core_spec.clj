(ns interpreter.core-spec
  (:require
    [speclj.core :refer :all]
    [interpreter.core :refer :all]))

(describe
  "interpretation"
  (it "interprets with function definition"
    (should=
      49
      (interpret-main
        {:square '(x (:times x x))
         :main '(:square 7)}))
    (should=
      16
      (interpret-main
        {:square '(x (:times x x))
         :main '(:square (:plus 2 2))}))
    (should=
      16
      (interpret-main
        {:increment '(n (:plus n 1))
         :square '(x (:times x x))
         :main '(:square (:plus 2 (:increment 1)))})))

  (it "interprets with variadic function definition"
    (should=
      4
      (interpret-main
        {:double-sum '(a b (:times 2 (:plus a b)))
         :main '(:double-sum 1 1)}))
    (should=
     18
      (interpret-main
        {:square '(a (:times a a))
         :double-square '(a a (:times 2 (:square a a)))
         :increment '(a (:plus a 1))
         :main '(:double-square (:increment 2))})))

  (it "interprets with if-based control flow"
     (should=
       1
       (interpret-main
         {:main '(:if true 1 0)}))
     (should=
       0
       (interpret-main
         {:main '(:if false 1 0)})))
  (it "does factorial"
      (should=
        (* 5 4 3 2 1)
        (interpret-main
          {:decrement '(a (:plus a -1))
           :factorial '(n (:if (:equals 1 n) n (:times n (:factorial (:decrement n)))))
           :main '(:factorial 5)}
          ))))
