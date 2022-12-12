(ns clojure-labs.lab_3.lab_3_2)

(defn split-list [split-size coll]
  (if (empty? coll)
    (list)
    (lazy-seq (cons (take split-size coll) (split-list split-size (drop split-size coll))))))

(defn collect-list [coll]
  (if (empty? coll)
    (list)
    (lazy-cat (first coll) (collect-list (rest coll)))))

(defn parallel-filter [pred parallel-size coll]
  (->> coll
       (split-list parallel-size)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (collect-list)
       (doall)))

(defn parallel-lazy-filter [pred split-size num-packs coll]
  (let [take-count (* split-size num-packs)]
    (lazy-cat (parallel-filter pred split-size (take take-count coll))
              (parallel-lazy-filter pred split-size num-packs (drop take-count coll)))))

(defn -main []
  (let [size 1000
        split-size 200
        num-packs 5
        pred #(or
                (Thread/sleep 10)                           ; типа долгий предикат
                (even? %))]
    (println "linear filter:")
    (println (count (->> (range)
                         (filter pred)
                         (take size)
                         (doall)
                         (time)
                         )))
    (println "parallel lazy filter:")
    (println (count (->> (range)
                         (parallel-lazy-filter pred split-size num-packs)
                         (take size)
                         (doall)
                         (time)
                         )))
    ))
