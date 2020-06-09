(ns iniciando.peg-thing
  (:require [clojure.set :as set]))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Genera una secuencia perezosa de numeros triangulares"
  ([] (tri* 0 1))
  ([sum n]
    (let [new-sum (+ sum n)]
      (cons new-sum (lazy-seq (tri*  new-sum (inc n)))))))


(def tri (tri*))


(defn triangular?
  "Es el numero triangular? e.j. 1, 3, 6, 10, 15"
  [n]
  (= n (last (take-while #(>= n %) tri))))

;(triangular? 3)

(defn row-tri
  "El numero triangular al final de la fila n"
  [n]
  (last (take n tri)))

(row-tri 12)

(take 12 tri)

(defn row-num
  "Devuelve el número de fila al que pertenece la posición: pos 1 en la fila 1,
  posiciones 2 y 3 en la fila 2, etc."
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Para una conexion mutua entre 2 posiciones"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]]) board))


(connect {} 15 1 2 4)

(assoc-in {} [[1 4] :connections [4 1]] 2)



(reduce (fn [new-board [p1 p2]]
          (assoc-in new-board [p1 :connections p2] 2))
        {}
        [[1 4] [4 1]])

