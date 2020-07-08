(ns iniciando.codesignal.maximal-square)

(def matrix [["1"] ["1"]])

(defn get-cuadrado [matrix x y n]
  (let [cuadrado (reduce (fn [cuadrado row] (conj cuadrado (take n (drop x row)))) [] (take n (drop y matrix)))]
    (if (= (count cuadrado) (count (first cuadrado))) cuadrado []) ))

(defn fill-posiciones? [matrix]
  (if (empty? matrix)
    false
    (reduce (fn [x y] (if (false? x) x (not (some #(= "0" %) y)))) true matrix)))

(defn subcuadrados [matrix x y n maximo]
  (let [max-x (count (first matrix))
        max-y (count matrix)]
    (cond
      (and (> (+ x n) max-x) (> (+ y n) max-y)) maximo
      (fill-posiciones? (get-cuadrado matrix x y n))
      (if (> (* n n) maximo) (recur matrix x y (inc n) (* n n)) (recur matrix x y (inc n) maximo))
      (> max-x (+ x n)) (recur matrix (inc x) y 1 maximo)
      (> max-y (+ y n)) (recur matrix 0 (inc y) 1 maximo)
      :else maximo)))

(defn maximalSquare [matrix]
  (subcuadrados matrix 0 0 1 0))

(maximalSquare (repeat 100 (repeat 100 "1")))