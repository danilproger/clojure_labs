(ns clojure-labs.lab_1.lab_1_1)

(defn add-elem-to-perm
  [perm elems]
  (if (empty? elems) (list)
    (if (not= (first elems) (first perm))
      (conj (add-elem-to-perm perm (rest elems)) (conj perm (first elems)))
      (add-elem-to-perm perm (rest elems)))))

(defn make-perms
  [perms elems]
  (if (empty? perms) (list)
    (concat (make-perms (rest perms) elems) (add-elem-to-perm (first perms) elems))))

(defn permutation
  [elems n]
  (if (<= n 0)
    (list (list))
    (make-perms (permutation elems (dec n)) elems)))

(defn -main
  []
  (println
    (permutation '(:a :b [:l [:c]]) 2)))
