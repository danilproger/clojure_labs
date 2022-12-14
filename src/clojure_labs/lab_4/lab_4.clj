(ns clojure-labs.lab_4.lab_4
  (:require [clojure-labs.lab_4.rules :refer :all]
            [clojure-labs.lab_4.types :refer :all]
            [clojure-labs.lab_4.utils :refer :all]
            [clojure-labs.lab_4.operations :refer :all]))

(defn build-dnf-logs [expr sign]
  (do
    (println "-----------------------")
    (println "sign: " (clojure.string/join " " (map #(str (first %) " = " (first (rest %)) ) sign)))
    (println "before: " (expr-to-str expr))
    (println "after: " (expr-to-str (build-dnf expr nil)))
    (when (not (= sign nil))
      (println "after with sign: " (expr-to-str (build-dnf expr sign))))
    (println "-----------------------\n")))

(defn -main []
  (let [x (variable :x)
        y (variable :y)
        z (variable :z)
        const_false (constant false)
        expr1 (peirce-arrow x x)
        expr2 (peirce-arrow (peirce-arrow x y) (peirce-arrow x y))
        expr3 (negative (conjunction (disjunction x (negative x)) (implication const_false z)))
        expr4 (negative (disjunction (conjunction (negative x) (negative y)) (conjunction (negative y) (negative z))))
        expr5 (negative (disjunction (implication x y) (negative (implication y z))))
        expr6 (disjunction (negative (disjunction (negative x) (negative y))) (conjunction x (negative y) expr5))
        sign {:x true, :y false, :z true}]

    (build-dnf-logs expr1 nil)
    (build-dnf-logs expr2 {:x true})
    (build-dnf-logs expr3 sign)
    (build-dnf-logs expr4 {:y false})
    (build-dnf-logs expr5 {:y false})
    (build-dnf-logs expr6 {:y false})
    ))