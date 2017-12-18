(ns euler-clj.euler81)

(require '[clojure.string :refer (join trim split split-lines)])

(defn file->strings
  "Takes as input a file and outputs a sequence of strings as a one string per line"
  [file]
  (clojure.string/split-lines
   (slurp file)))

(defn strings->vectors
  "Takes as input a sequence of strings and outputs a sequence of vectors"
  ([strings] (string->vectors strings #", *"))
  ([strings delim]
   (into []
         (for [line strings]
           (into [] (map read-string
                         (clojure.string/split line delim)))))))

(defn init-least-weights
  "Initializes a 2-D array of the same size as input with values set to 0"
  [weights]
  (let [s (count weights)]
    (to-array-2d
     (repeat s (repeat s 0)))))

(defn make-weights
  "creates a java array from a sequence vectors"
  [vectors]
  (to-array-2d vectors))

(defn find-least-weight
  "Takes initial matrix w and results container lw and finds the least weight and updates lw[i][j]"
  [w lw]
  (let [s (count w)]
    (aset lw 0 0 (aget w 0 0)) ;; lw[0][0] = w[0][0]
    (doseq [i (range 1 s)]
      (aset lw i 0 (+ (aget w i 0) (aget lw (dec i) 0)))) ;; lw[i][0] = w[i][0] + lw[i - 1][0]
    (doseq [j (range 1 s)]
      (aset lw 0 j (+ (aget w 0 j) (aget lw 0 (dec j))))) ;; lw[0][j] = w[0][j] + lw[0][j - 1]
    (doseq [i (range 1 s), j (range 1 s)
            :let [least (min (aget lw i (dec j)) (aget lw (dec i) j))]]
      (aset lw i j (+ (aget w i j) least))) ;; lw[i][j] = w[i][j] + min(lw[i][j - 1], lw[i - 1][j])
    ))

(defn test []
  (def vs-test
    (strings->vectors
     ["131,673,234,103,18"
      "201,96,342,965,150"
      "630,803,746,422,111"
      "537,699,497,121,956"
      "805,732,524,37,331"]))
  (def w-test (make-weights vs-test))
  (def lw-test (init-least-weights w-test))
  (find-least-weight w-test lw-test)
  lw-test)

(defn euler81
  []
  (def w (make-weights
          (strings->vectors (file->strings "src/euler_clj/euler81_input.txt"))))
  (def lw (init-least-weights w))
  (find-least-weight w lw)
  (aget lw (dec (count lw)) (dec (count lw))))

(euler81)
