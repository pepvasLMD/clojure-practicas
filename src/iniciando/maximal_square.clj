(ns iniciando.maximal-square)

(def matrix [[1, 0, 1, 1, 1]
              [1, 0, 1, 1, 1],
               [1, 1, 1, 1, 1],
                [1, 0, 0, 1, 0],
                 [1, 0, 0, 1, 0]])


(defn get-cuadrado [matrix i f n]
  (reduce (fn [x y] (conj x (take f (drop i y)))) [] (take n (drop i matrix))))

(defn fill-posiciones? [matrix]
  (= 0 (reduce (fn [x y] (+ x (count (filter #(= 0 %) y)))) 0 matrix)))

(defn maximalSquare [matrix])


(get-cuadrado matrix 2 0)
(fill-posiciones? (get-cuadrado matrix 0 2))