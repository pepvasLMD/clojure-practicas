(ns iniciando.reacomodamiento-string)


(defn rotations [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn rotaciones [secuencia]
  (reduce (fn [x y]
            (let [ultimo (last x)]
                      (conj x (conj (butlast ultimo) (last ultimo))))) [secuencia] (rest secuencia)))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (mapcat
      (fn [[x & xs]] (map #(cons x %) (permutations xs)))
      (rotaciones a-set))))

(defn stringsRearrangement [s]
  (->> s
       permutations
       (some
         #(reduce
            (fn [acc itm]
              (if (= 1 (count (filter false? (map = acc itm))))
                itm (reduced false))) %))
       boolean))


(defn character-dif [x y]
  (let [zip (map vector x y)]
    (reduce (fn [x y] (if (identical? (first y) (second y)) x (inc x))) 0 zip )))

(character-dif "aba" "aba")

(defn comprobar [seq-a]
  (let [ x (permutations seq-a)]
    x))

(comprobar ["aba"
               "bbb"
               "bab"])



(permutations ["aba"
               "bbb"])

(let [coll ["aba" "bbb" "bab"]]
  (for [()]))


(rotations ["aba"
            "bbb"
            "bab"])

(permutations ["aba"
               "bbb"
               "bab"])

(stringsRearrangement ["aba"
                       "bbb"
                       "bab"])