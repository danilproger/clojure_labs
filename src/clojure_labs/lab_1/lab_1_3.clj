(ns clojure-labs.lab_1.lab_1_3)

(defn my-map
  [fun x]
  (letfn [(map-f [acc val] (concat acc (list (fun val))))]
    (reduce map-f (list) x)))

(defn my-map-no-reduce
  ([fun x] (my-map-no-reduce fun x (list)))
  ([fun x acc]
   (if (empty? x)
     acc
     (recur fun (rest x) (concat acc (list (fun (first x))))))))

(defn my-filter
  [pred x]
  (letfn [(fil-f [acc val] (if (pred val) (concat acc (list val)) acc))]
    (reduce fil-f (list) x)))

(defn my-filter-no-reduce
  ([pred x] (my-filter-no-reduce pred x (list)))
  ([pred x acc]
   (if (empty? x)
     acc
     (recur pred (rest x) (if (pred (first x)) (concat acc (list (first x))) acc)))))

(defn -main []
  (println (my-filter (fn [x] (odd? x)) (list 1 2 3 7 8)))
  (println (my-filter-no-reduce (fn [x] (odd? x)) (list 1 2 3 7 8)))
  (println (my-map (fn [x] (* x x x)) (list 1 2 3 7 8)))
  (println (my-map-no-reduce (fn [x] (* x x x)) (list 1 2 3 7 8)))
  )
