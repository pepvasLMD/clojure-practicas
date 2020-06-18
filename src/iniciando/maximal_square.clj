(ns iniciando.maximal-square)

(def matrix [["1","0","1","1","1"],
             ["1","0","1","1","1"],
             ["1","1","1","1","1"],
             ["1","0","0","1","0"],
             ["1","0","0","1","0"]])


(defn get-cuadrado [matrix xi yi xf yf]
  (reduce (fn [x y] (conj x (take xf (drop xi y)))) [] (take yf (drop yi matrix))))

(defn fill-posiciones? [matrix]
  (reduce (fn [x y] (if (false? x) x (not (some #(= 0 (int %)) y)))) true matrix))

(defn get-permutaciones [matrix]
  (let [max-y (count matrix)
         max-x (count (first matrix))]
    (for [x (range max-x)
          y (range max-y)
          xf (range max-x)
          yf (range max-y)]
      [x y xf yf])))

(defn area-matrix [matrix]
  (let [num-rows (count matrix)
         num-cols (count (first matrix))]
    (* num-rows num-cols)))

(defn maximalSquare [matrix]
  (let [perm (get-permutaciones matrix)
        cuadrados (map (fn [x] (get-cuadrado matrix (x 0) (x 1) (x 2) (x 3))) perm)
        areas (map area-matrix (filter fill-posiciones? cuadrados))]
    (if (empty? areas) 0 (reduce max areas))))

(maximalSquare[])
(fill-posiciones?  matrix)