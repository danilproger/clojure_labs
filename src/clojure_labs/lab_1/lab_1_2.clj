(ns clojure-labs.lab_1.lab_1_2)

(defn can-be-added?
  [l1 l2]
  (not= (first l1) (first l2)))

(defn elems-to-list
  ([elems] (elems-to-list elems (list)))
  ([elems acc]
   (if (empty? elems)
     acc
     (recur (rest elems) (conj acc (list (first elems)))))))

(defn add-elem-to-perm
  ([perm elems] (add-elem-to-perm perm elems (list)))
  ([perm elems acc]
   (if (empty? elems)
     acc
     (if (can-be-added? elems perm)
       (recur perm (rest elems) (conj acc (conj perm (first elems))))
       (recur perm (rest elems) acc)))))

(defn make-perms
  ([perms elems] (make-perms perms elems (list)))
  ([perms elems acc]
   (if (empty? perms)
     acc
     (recur (rest perms) elems (concat (add-elem-to-perm (first perms) elems) acc)))))

(defn permutation
  ([elems n] (permutation elems n (elems-to-list elems)))
  ([elems n acc]
   (if (<= n 1)
     acc
     (recur elems (dec n) (make-perms acc elems)))))


(defn -main
  []
  (println
    (permutation '(:a :b [:l [:c]]) 2)))
