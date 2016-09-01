(ns euler-clj.euler75
  (:require [clojure.math.numeric-tower :as math]))

(defn triples
  [n]
  (for [u (range 1 (math/round (math/sqrt n)))
        v (cons (+ u 1) (range (+ u 3) (- (math/round (math/sqrt n)) u)))
        :let [a (- (math/expt v 2) (math/expt u 2))
                b (* 2 u v)
                c (+ (math/expt v 2) (math/expt u 2))]
          :when (== (math/gcd u v) 1)]
          (+ a b c)))

(triples 100)

(defn triples'
  [n]
  (for [m (range 2 (math/round (math/sqrt (math/floor (/ n 2)))))
        n (range 1 m)
        :when (== (mod (+ m n) 2) 1)
        :when (== (math/gcd n m) 1)]
        [(- (* m n) (* n n)) (* 2 (* m n)) (+ (* m n)(* n n))]))

(triples' 100)

(defn nbtriples'
  [n]
  (count (triples' n)))

(nbtriples' 1500000)

(defn nbtriples''
  [n]
  (reduce + (map first (triples' n))))

(nbtriples'' 1500000)

