(ns clojure-labs.lab_4.operations
  (:require [clojure-labs.lab_4.types :refer :all]))

(defn args
  "Return arguments of expression."
  [expr]
  (rest expr))

(defn disjunction?
  "Return true if expression is disjunction.
   In other case - false."
  [expr]
  (= (get-type expr) ::disj))

(defn disjunction
  "Return disjunction if 2 or more expressions in disjunction."
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::disj (cons expr rest))))

(defn conjunction?
  "Return true if expression is conjunction.
   In other case - false."
  [expr]
  (= (get-type expr) ::conj))

(defn conjunction
  "Return conjunction if 2 or more expressions in conjunction."
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::conj (cons expr rest))))

(defn negative?
  "Return true if expression is negative.
   In other case - false."
  [expr]
  (= (get-type expr) ::neg))

(defn negative
  "Return negative of expression."
  [expr]
  (list ::neg expr))

(defn neg-inner-arg
  "Return arg of neg expr."
  [neg-expr]
  {:pre [(negative? neg-expr)]}
  (first (args neg-expr)))

(defn implication?
  "Return true if expression is implication.
   In other case - false."
  [expr]
  (= (get-type expr) ::impl))

(defn implication
  "Return implication of 2 expressions."
  [antecedent consequent]
  (list ::impl antecedent consequent))

(defn impl-ant
  "Return first argument of implication - antecedent."
  [impl-expr]
  {:pre [(implication? impl-expr)]}
  (first (args impl-expr)))

(defn impl-cons
  "Return second argument of implication - consequent."
  [impl-expr]
  {:pre [(implication? impl-expr)]}
  (first (rest (args impl-expr))))

(defn peirce-arrow?
  "Return true if expression is peirce-arrow.
   In other case - false."
  [expr]
  (= (get-type expr) ::pierce))

(defn peirce-arrow
  "Return Peirce's arrow expression of 2 expressions."
  [expr1 expr2]
  (list ::pierce expr1 expr2))
