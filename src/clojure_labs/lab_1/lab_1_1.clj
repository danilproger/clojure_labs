(ns clojure-labs.lab_1.lab_1_1)

(defn can-be-added?
  [l1 l2]
  (not= (first l1) (first l2)))

(defn elems-to-list
  [elems]
  (if (empty? elems) (list)
    (conj (elems-to-list (rest elems)) (list (first elems)))))

(defn add-elem-to-perm
  [perm elems]
  (if (empty? elems) (list)
    (if (can-be-added? elems perm)
      (conj (add-elem-to-perm perm (rest elems)) (conj perm (first elems)))
      (add-elem-to-perm perm (rest elems)))))

(defn make-perms
  [perms elems]
  (if (empty? perms) (list)
    (concat (make-perms (rest perms) elems) (add-elem-to-perm (first perms) elems))))

(defn permutation
  [elems n]
  (if (<= n 1)
    (elems-to-list elems)
    (make-perms (permutation elems (dec n)) elems)))


(defn -main
  []
  (println
    (permutation '(:a :b [:l [:c]]) 2)))
