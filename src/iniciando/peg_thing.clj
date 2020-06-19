(ns iniciando.peg-thing
  (:require [clojure.set :as set]
            [clojure.string :as s]))

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

(row-tri 2)

(take 100 tri)

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
(assoc-in (connect {} 15 1 2 4) [ 1 :connections 2 1] 22)





(assoc-in {} [:cookie :monster :vocals] "Finntroll")

(get-in {:cookie {:monster {:vocals "Finntroll"}}} [:cookie :monster])


(assoc-in {} [1 :connections 4] 2)

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))



(defn connect-down-left
  [board max-pos pos]
    (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(connect-down-left {} 15 2)

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))


(connect-down-left {} 15 1)
(connect-right {} 15 4)


(defn add-pos
  "Establece la posicion y realiza conexiones"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))


(add-pos {} 15 2)


(assoc-in {} [1 :pegged] true)


(assoc-in (assoc-in {} [1 :pegged] true) [1 :pegged] 2)

(connect-right (assoc-in {} [1 :pegged] true) 15 1)

(connect-down-left (connect-right (assoc-in {} [1 :pegged] true) 15 1) 15 1)

(connect-down-right (connect-right (connect-down-left (assoc-in {} [1 :pegged] true) 15 1) 15 1) 15 1)



(defn clean
  [text]
  (reduce (fn [string string-fn] (string-fn string))
          text
          [s/trim #(s/replace % #"lol" "LOL")]))

(clean "asdfsaloldf")




(defn new-board
  "Crea un nuevo tablero dando el numero de filas"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))


(new-board 5)


(defn pegged?
  "La posicion tiene una clavija?"
  [board pos]
  (get-in board [pos :pegged]))


(defn remove-peg
  "Retira la clavija del tablero"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Pone la clavija en el tablero dada una posicion"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Mueve una clavija del lugar p1 al lugar p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))


(defn valid-moves
  "Devuelve un mapa de todos los movimientos validos para una posicion,
  donde la clave es el destino y el valor es la posicion saltada"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))


(def my-board (assoc-in (new-board 5) [4 :pegged] false))


(defn valid-move?
  "Devuelve la posicion saltada del movimiento p1 a p2 si es valido, nil en otro caso"
  [board p1 p2]
  (get (valid-moves board p1) p2))



(defn make-move
  "Mueve una clavija de la posicion p1 a p2, removiendo la clavija saltada"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))



(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(range alpha-start alpha-end)



(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0"                                                ;(colorize "0" :blue)
         "-"                                                ;(colorize "-" :red)
         )))

(defn row-positions
  "Devuelve todas las posiciones de la fila dada"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))


(defn row-padding
  "Cadena de espacios para agregar en el principio de la fila hasta el centro"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))


(row-padding 1 5)



(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))



(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))




(defn letter->pos
  "Convierte una letra de una cadena al numero de posicion correspondiente"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(letter->pos "a")


(defn get-input
  "Espera a que el usuario ingrese el texto y presione enter, luego limpia la entrada"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))


;(get-input)

(defn characters-as-strings
  [cadena]
  (filter #(not= " " %1) (map (comp str char) cadena)))


(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))


(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))


(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))



(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))
