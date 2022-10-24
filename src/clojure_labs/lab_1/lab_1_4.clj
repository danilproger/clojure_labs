(ns clojure-labs.lab_1.lab_1_4)

(defn add-elem-to-perm
  [perm elems]
  (map (fn [elem] (conj perm elem)) (filter (fn [elem] (not= (first perm) elem)) elems)))

(defn make-perms
  [perms elems]
  (reduce concat (map (fn [perm] (add-elem-to-perm perm elems)) perms)))

(defn permutation
  [elems n]
  (nth (iterate (fn [perm] (make-perms perm elems)) (list (list))) n))

(defn -main
  []
  (println
    (permutation '(:a :b [:l [:c]]) 2)))
