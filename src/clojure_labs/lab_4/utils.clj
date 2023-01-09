(ns clojure-labs.lab_4.utils
  (:require [clojure-labs.lab_4.types :refer :all]
            [clojure-labs.lab_4.operations :refer :all]))

(defn expr-to-str [expr]
  (cond
    (constant? expr) (str (constant-value expr))
    (variable? expr) (str (variable-name expr))
    (disjunction? expr) (str "(" (clojure.string/join " v " (map #(expr-to-str %) (args expr))) ")")
    (conjunction? expr) (str "(" (clojure.string/join " & " (map #(expr-to-str %) (args expr))) ")")
    (implication? expr) (str "(" (expr-to-str (impl-ant expr)) " -> " (expr-to-str (impl-cons expr)) ")")
    (negative? expr) (str "!" (expr-to-str (first (args expr))) "")
    (peirce-arrow? expr) (str "(" (expr-to-str (first (args expr))) " ↓ " (expr-to-str (first (rest (args expr)))) ")")
    ))

;equality
(declare deep-equals)

(defn inner-coll-type-equality
  ([exprs1 exprs2]
   (cond
     (or (and (empty? exprs1) (not (empty? exprs2))) (and (empty? exprs2) (not (empty? exprs1)))) false
     (empty? exprs1) true
     :else (let [single1 (first exprs1)
                 another-exprs-1 (remove #(deep-equals % single1) exprs1)
                 another-exprs-2 (remove #(deep-equals % single1) exprs2)]
             (recur another-exprs-1 another-exprs-2))
     )))

(defn deep-equals
  ([expr1 expr2]
   (cond
     (not (= (get-type expr1) (get-type expr2))) false
     (constant? expr1) (= (constant-value expr1) (constant-value expr2))
     (variable? expr1) (same-variables? expr1 expr2)
     (negative? expr1) (deep-equals (neg-inner-arg expr1) (neg-inner-arg expr2))
     (implication? expr1) (and (deep-equals (impl-ant expr1) (impl-ant expr2))
                               (deep-equals (impl-cons expr1) (impl-cons expr2)))
     (disjunction? expr1) (inner-coll-type-equality (args expr1) (args expr2))
     (conjunction? expr1) (inner-coll-type-equality (args expr1) (args expr2))
     :else false)))

;Remove repeats
(defn get-distinct-exprs
  [exprs]
  (loop
    [exprs exprs
     acc (list)]
    (if (empty? exprs)
      acc
      (let [single (first exprs)
            another-exprs (remove #(deep-equals % single) exprs)]
        (recur another-exprs (concat acc (list single)))))))

;Check if has opposite exprs: x & !x, x v !x
(defn has-opposite? [exprs]
  (let [dist-exprs (get-distinct-exprs exprs)
        neg-exprs (filter negative? dist-exprs)
        inner-neg-exprs (map #(neg-inner-arg %) neg-exprs)
        another-exprs (remove negative? dist-exprs)
        dist-union (get-distinct-exprs (concat inner-neg-exprs another-exprs))]
    (if (or (empty? inner-neg-exprs) (empty? another-exprs))
      false
      (not (= (count dist-union) (+ (count inner-neg-exprs) (count another-exprs)))))))


(defn cart-prod-2 [coll1 coll2]
  (reduce
    (fn [acc1 c1]
      (concat acc1 (reduce
                     (fn [acc2 c2]
                       (concat acc2 (list (concat c1 c2))))
                     (list)
                     coll2)))
    (list)
    coll1))

(defn cart-prod
  [colls]
   (if (empty? colls)
     (list (list))
     (reduce
       (fn [acc e]
         (if (empty? acc)
           (map list e)
           (cart-prod-2 acc (map list e))))
       (list)
       colls)))