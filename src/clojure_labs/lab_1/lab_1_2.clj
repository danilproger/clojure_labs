(ns clojure-labs.lab_1.lab_1_2)

(defn add-elem-to-perm
  [perm elems]
  (loop [acc (list) elems elems]
    (if (empty? elems)
      acc
      (if (not= (first elems) (first perm))
        (recur (conj acc (conj perm (first elems))) (rest elems))
        (recur acc (rest elems))))))

(defn make-perms
  [perms elems]
  (loop [acc (list) perms perms]
    (if (empty? perms)
      acc
      (recur (concat (add-elem-to-perm (first perms) elems) acc) (rest perms)))))

(defn permutation
  [elems n]
  (loop [acc (list (list)) n n]
    (if (<= n 0)
      acc
      (recur (make-perms acc elems) (dec n)))))

(defn -main
  []
  (println
    (permutation '(:a :b [:l [:c]]) 2)))
