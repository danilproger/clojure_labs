(ns clojure-labs.lab_2.lab_2_1)

(defn trapeze [f x delta]
  (* (/ (+ (f x) (f (+ x delta))) 2) delta))

(defn integrate
  [f begin end delta]
  (reduce
    +
    0.0
    (map (fn [x] (trapeze f x delta)) (range begin end delta))))

(defn integrate-memo [fun dx]
  (let [fun-memo (memoize (fn [fr f dx x]
                               (if (<= x 0.0)
                                 0.0
                                 (+ (fr fr f dx (- x dx)) (trapeze f (- x dx) dx)))
                             ))]
    #(fun-memo fun-memo fun dx (* dx (/ % dx)))))

(defn -main []
  (let [fun #(* % %)
        dx 1.5
        x 20
        fun-memo (integrate-memo fun dx)]
    (println (time (integrate fun 0 x dx)))
    (println "memo:")
    (println (time (fun-memo x)))
    (println (time (fun-memo x)))
    (println (time (fun-memo (+ x 5))))
    (println (time (fun-memo (- x 2))))
    (println (time (fun-memo (+ x 2))))
    (println (time (fun-memo (- x 1)))))
  )
