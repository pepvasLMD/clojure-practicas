(ns iniciando.lines-game)


(def nums [0 1 2 3 5 2])


(defn rotaciones [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn zip [x y]
  (map vector x y))

(defn recursivo [nums k]
  (if (empty? nums)
    false
    (let [indice-valor (first nums)
          valor (second indice-valor)
          indices (map first (take-while #(== valor (second %)) nums))
          diferencias (map #(- (second %) (first %)) (zip indices (rest indices)))]
      (if (some #(<= % k) diferencias) true (recursivo (drop-while #(== valor (second %)) nums) k) ))))

(defn containsCloseNums [nums k]
  (let [x (zip (range (count nums)) nums)
        ordenado (sort-by second x)]
    (recursivo ordenado k)))


(containsCloseNums nums 3)



