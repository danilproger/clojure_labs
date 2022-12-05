(ns clojure-labs.lab_3.lab_3_1)

(defn split-list [split-size coll]
  (loop [acc (list)
         coll coll]
    (if (empty? coll)
      acc
      (recur (concat acc (list (take split-size coll))) (drop split-size coll)))))

(defn collect-list [coll]
  (loop [acc (list)
         coll coll]
    (if (empty? coll)
      acc
      (recur (concat acc (first coll)) (rest coll)))))

(defn parallel-filter [pred parallel-size coll]
  (->> coll
       (split-list parallel-size)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (doall)
       (collect-list))
  )

(defn -main []
  (let [coll (range 1000)
        thread-count (.availableProcessors (Runtime/getRuntime))
        split-size (/ (count coll) thread-count)
        pred #(or
                (Thread/sleep 10)                           ; типа долгий предикат
                (even? %))]
    (println "linear filter:")
    (println (time (count (filter pred coll))))
    (println "parallel filter:")
    (println (time (count (parallel-filter pred split-size coll))))))
