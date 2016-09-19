(ns euler-clj.euler30
  (:require [clojure.math.numeric-tower :as math]))

(defn digits
  [n]
  (->> n str (map (comp read-string str))))

;; or writing it without threading macro
(defn digits2
  [n]
  (map (comp read-string str) (str n)))

(digits 1234)
(digits2 1234)

;; another way to write it
(defn digits3
  [n]
  (if (pos? n)
    (conj (digits3 (quot n 10)) (mod n 10))
    []))

(digits3 1234)

;; using iterate
(defn digits4
  [n]
  (->> n
  (iterate #(quot % 10))
  (take-while pos?)
  (mapv #(mod % 10))
  rseq))

;; (defn euler30
;;   "Find the sum of all numbers that can be written the sum of theirs n'th powers of their digits."
;;   [n]
;;   (range 2..999999)
;;   )

;; (take 10 (reduce + (map #(math/exp % 5) (digits x))) == x)

;; (take 20 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))

;;  [x | x <- [2..999999], sum (map (^5) (digs x)) == x]

(defn euler30
  []
  (reduce +
          (for [x (range 2 999999)
                :when (= (reduce + (map #(math/expt % 5) (digits x))) x)]
            x)))

;; (euler30)
