(ns clojure-labs.lab_2.lab_2_2)

(defn trapeze [f x delta]
  (* (/ (+ (f x) (f (+ x delta))) 2) delta))

(defn integrate
  [f begin end delta]
  (reduce
    +
    0.0
    (map (fn [x] (trapeze f x delta)) (range begin end delta))))

(defn integrate-seq [fun dx]
  (letfn [(seq [fun dx]
            (map first
                 (iterate (fn [[acc x]]
                            [(+ acc (trapeze fun x dx)) (+ x dx)])
                          [0 0])))]
    #(nth (seq fun dx) (/ % dx))))

(defn -main []
  (let [fun #(* % %)
        dx 1.5
        x 20
        fun-seq (integrate-seq fun dx)]
    (println (time (integrate fun 0 x dx)))
    (println "seq:")
    (println (time (fun-seq x)))
    (println (time (fun-seq x)))
    (println (time (fun-seq (+ x 5))))
    (println (time (fun-seq (- x 2))))
    (println (time (fun-seq (+ x 2))))
    (println (time (fun-seq (- x 1)))))
  )
