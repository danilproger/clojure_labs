(ns clojure-labs.lab_4.types)

(defn get-type
  "Return type of expression."
  [expr]
  (first expr))

(defn constant
  "Return constant expression with true or false."
  [bool]
  {:pre [(or (= bool true) (= bool false))]}
  (list ::const bool))

(defn constant?
  "Return true if expression is constant.
   In other case - false."
  [expr]
  (= (get-type expr) ::const))

(defn constant-value
  "Return boolean value of constant."
  [expr]
  {:pre [(constant? expr)]}
  (second expr))

(defn variable
  "Return variable expression."
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Return true if expression is variable.
   In other case - false."
  [expr]
  (= (get-type expr) ::var))

(defn variable-name
  "Return variable name."
  [expr]
  {:pre [(variable? expr)]}
  (second expr))

(defn same-variables?
  "Check if 2 variables are equivalent."
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= (variable-name v1)
          (variable-name v2))))
