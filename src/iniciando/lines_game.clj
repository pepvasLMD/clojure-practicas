(ns iniciando.lines-game)


(def nums [0 1 2 3 5 2])


(defn rotaciones [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn containsCloseNums [nums k]
  (let [x (map vector (range (count nums)) nums)]
    (into (sorted-map) x)))

(rotaciones (containsCloseNums nums 2))

