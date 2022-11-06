(ns clojure-labs.lab_2.lab_2_1)

(defn trapeze [f x delta]
  (* (/ (+ (f x) (f (+ x delta))) 2) delta))

(defn count-integrate
  [f begin end delta]
  (reduce
    +
    0.0
    (map (fn [x] (trapeze f x delta)) (range begin end delta))))

(defn integrate-integer
  ([f x delta]
   (loop [x x acc 0]
     (if (= x 0)
       acc
       (recur (dec x) (+ acc (count-integrate f (dec x) x delta)))))))

(defn integrate [f x delta]
  (+ (count-integrate f (int x) x delta)
     (integrate-integer f (int x) delta)))

(def memoized-integrate-integer
  (memoize (fn [f x delta]
             (if (= x 0)
               0
               (+ (memoized-integrate-integer f (dec x) delta) (count-integrate f (dec x) x delta))))))

(defn memoized-integrate [f x delta]
  (+ (count-integrate f (int x) x delta)
     (memoized-integrate-integer f (int x) delta)))

(defn fun [x] (* x x x))

(def dx 0.0001)
(def x 100.7)

(defn -main []
  (println (time (integrate fun x dx)))
  (println "memoized:")
  (println (time (memoized-integrate fun x dx)))
  (println (time (memoized-integrate fun (+ x 1) dx)))
  (println (time (memoized-integrate fun (+ x 2) dx)))
  (println (time (memoized-integrate fun (- x 2) dx)))
  )
