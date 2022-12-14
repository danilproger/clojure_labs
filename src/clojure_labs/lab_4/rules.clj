(ns clojure-labs.lab_4.rules
  (:require [clojure-labs.lab_4.utils :refer :all]
            [clojure-labs.lab_4.types :refer :all]
            [clojure-labs.lab_4.operations :refer :all]))

(defn enroll-same [pred? exprs]
  (reduce
    (fn [acc expr]
      (if (pred? expr)
        (concat acc (args expr))
        (cons expr acc)))
    (list)
    exprs))

(declare build-dnf)

;Rules for constants
(defn constant-rules
  [expr _]
  expr)

;Rules for variables
(defn variable-rules
  [expr sign]
  (let [val ((variable-name expr) sign)]
    (if (= val nil)
      expr
      (constant val))))

;Rules for implications
(defn implication-rules [expr sign]
  (let [ant (build-dnf (impl-ant expr) sign)
        con (build-dnf (impl-cons expr) sign)]
    (cond
      (and (constant? con) (= (constant-value con) true)) (constant true)
      (and (constant? ant) (= (constant-value ant) false)) (constant true)
      (and (constant? ant) (= (constant-value ant) true) (constant? con) (= (constant-value con) false)) (constant false)
      :else (disjunction (negative ant) con)
      )))

;Rules for negatives
(defn negative-rules [neg-expr sign]
  (let [expr (build-dnf (neg-inner-arg neg-expr) sign)]
    (cond
      (constant? expr) (constant (not (constant-value expr)))
      (disjunction? expr) (build-dnf (apply conjunction (map #(build-dnf (negative %) sign) (args expr))) sign)
      (conjunction? expr) (build-dnf (apply disjunction (map #(build-dnf (negative %) sign) (args expr))) sign)
      (negative? expr) (build-dnf (neg-inner-arg expr) sign)
      :else (negative expr)
      )))

;Rules for conjunction
(defn distribute-disj-in-conj [conj-expr sign]
  (let [conj-args (args conj-expr)
        disj-exprs (filter disjunction? conj-args)
        non-disj-exprs (remove disjunction? conj-args)
        cart-prod-disj (cart-prod (map #(args %) disj-exprs))
        new-conj-args (map #(concat non-disj-exprs %) cart-prod-disj)]
    (build-dnf (apply disjunction (map #(apply conjunction %) new-conj-args)) sign)))

(defn conjunction-rules [conj-expr sign]
  (let [exprs (args conj-expr)
        built-exprs (doall (map #(build-dnf % sign) exprs))
        enrolled-exprs (enroll-same conjunction? built-exprs)
        dist-exprs (get-distinct-exprs enrolled-exprs)
        consts (filter constant? dist-exprs)
        other-exprs (remove constant? dist-exprs)
        collapsed-consts (reduce (fn [acc expr] (and acc (constant-value expr))) true consts)]
    (cond
      (= collapsed-consts false) (constant false)
      (empty? other-exprs) (constant true)
      (= (count other-exprs) 1) (first other-exprs)
      (has-opposite? other-exprs) (constant false)
      (some disjunction? other-exprs) (distribute-disj-in-conj (apply conjunction other-exprs) sign)
      :else (apply conjunction other-exprs)
      )))

;Rules for disjunction
(defn disjunction-rules [disj-expr sign]
  (let [exprs (args disj-expr)
        built-exprs (doall (map #(build-dnf % sign) exprs))
        enrolled-exprs (enroll-same disjunction? built-exprs)
        dist-exprs (get-distinct-exprs enrolled-exprs)
        consts (filter constant? dist-exprs)
        collapsed-consts (reduce (fn [acc expr] (or acc (constant-value expr))) false consts)
        other-exprs (remove constant? dist-exprs)]
    (cond
      (= collapsed-consts true) (constant true)
      (empty? other-exprs) (constant false)
      (= (count other-exprs) 1) (first other-exprs)
      (has-opposite? other-exprs) (constant true)
      :else (apply disjunction other-exprs)
      )))

(defn peirce-arrow-rules [peirce-arrow-expr sign]
  (build-dnf
    (negative
      (disjunction
        (first (args peirce-arrow-expr))
        (first (rest (args peirce-arrow-expr))))) sign))

(def build-dnf-rules
  (list
    [#(constant? %) constant-rules]
    [#(variable? %) variable-rules]
    [#(disjunction? %) disjunction-rules]
    [#(conjunction? %) conjunction-rules]
    [#(implication? %) implication-rules]
    [#(negative? %) negative-rules]
    [#(peirce-arrow? %) peirce-arrow-rules]
    ))


(defn build-dnf
  "Function creates DNF from expression."
  [expr sign]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         build-dnf-rules)
   expr sign))
