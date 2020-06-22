(ns iniciando.reacomodamiento-string)

(defn rotations [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (mapcat
      (fn [[x & xs]] (map #(cons x %) (permutations xs)))
      (rotations a-set))))


(defn stringsRearrangement [s]
  (->> s
       permutations
       (some
         #(reduce
            (fn [acc itm]
              (if (= 1 (count (filter false? (map = acc itm))))
                itm (reduced false))) %))
       boolean))

(stringsRearrangement ["aba", "bbb", "bab"])



(defn
  ^{:doc "mymax [xs+] gets the maximum value in xs using > "
    :test (fn []
            (assert (= 42  (mymax 2 42 5 4))))
    :user/comment "this is the best fn ever!"}
  mymax
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce mymax (mymax x y) more)))

