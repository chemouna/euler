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

(defn perimeters
  [n]
  )
