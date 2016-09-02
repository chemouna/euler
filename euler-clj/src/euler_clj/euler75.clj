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
        :when (and (== (mod (+ m n) 2) 1)
                   (== (math/gcd n m) 1)
                   (or (even? n) (even? m)))]
    [(- (* m n) (* n n)) (* 2 (* m n)) (+ (* m n)(* n n))]))

(triples' 100)

(defn nbtriples'
  [n]
  (count (triples' n)))

(nbtriples' 1500000)
;; 151804

(defn nbtriples''
  [n]
  (reduce + (map first (triples' n))))

(nbtriples'' 1500000)

;; it should return  161667 triangles which can be bent one way (and take an acceptable time around 45ms)

;; Second attempt
(defn coprime? [a b]
  (= (math/gcd a b)
     1))

(defn square [a]
  (* a a))

(defn pqs [max-perimeter]
  (let [upto (math/sqrt max-perimeter)]
    (print upto)
    (for [p (range (inc upto))
          q (range 1 p)
          :when (and (or (even? p) (even? q))
                     (coprime? p q))] ;;one of them needs to be odd and both need to be coprime
      [p q])))

(pqs 12)

(defn primative [max-perimeter]
  (for  [[p q] (pqs max-perimeter)
         :let [sqp (square p)
               sqq (square q)
               a (* 2 p q)
               b (- sqp sqq)
               c (+ sqp sqq)
               per (+ (* 2 p q)
                      (* 2 sqp))]
         ]
    [a b c per]))

(defn triples2 [max-perimeter]
  (for [t (primative max-perimeter)
        mult (iterate inc 1)
        :while (< (* (nth t 3) mult)
                  max-perimeter)]
    (vec (map #(* mult %) t))))

(triples2 25)

(time (println
       (count
        (filter #(= (val %)
                    1)
                (frequencies
                 (map #(nth % 3)
                      (triples2 1500000)))))))
