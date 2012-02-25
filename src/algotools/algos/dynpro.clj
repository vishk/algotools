(ns
 ^{:author "Vish Kohli",
   :doc "A set of widely-applicable (and fun!) algorithms based mainly on dynamic-programming.
        (To be augmented)
"}
    algotools.algos.dynpro)

(defn lev-dist
"Uses dynamic programming to calculate the Levenshtein dist (number of operations) to convert between strings a and b; allowed operations being insertion, deletion and single-character substitution."
[a b]
(let [X (cons \_ a)  Y (cons \_ b)  m (count X)  n (count Y)
      A (make-array Long/TYPE m n)]
  (dotimes [i m]
    (dotimes [j n]
      (aset A i j (cond (= 0 i) j
                        (= 0 j) i
                        :else (min (+ (aget A (dec i) (dec j))
                                      (if (= (nth X i) (nth Y j)) 0 1))
                                   (+ (aget A (dec i) j) 1)
                                   (+ (aget A i (dec j)) 1))))))
  (aget A (dec m) (dec n))))
