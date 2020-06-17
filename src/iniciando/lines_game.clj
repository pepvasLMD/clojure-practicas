(ns iniciando.lines-game)


(def nums [0 1 2 3 5 2])

(defn zip [x y]
  (map vector x y))

(defn recursivo [nums k]
  (if (empty? nums)
    false
    (let [indice-valor (first nums)
          valor (second indice-valor)
          indices (map first (take-while #(== valor (second %)) nums))
          diferencias (map #(- (second %) (first %)) (zip indices (rest indices)))]
      (if (some #(<= % k) diferencias) true (recur (drop-while #(== valor (second %)) nums) k) ))))

(defn containsCloseNums [nums k]
  (let [x (zip (range (count nums)) nums)
        ordenado (sort-by second x)]
    (recursivo ordenado k)))


(containsCloseNums (range 0 55000) 3)



(def field [['.', 'G', '.', '.', '.', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.', 'V', '.'],
            ['.', 'O', '.', '.', 'O', '.', '.', '.', '.'],
            ['.', '.', '.', '.', 'O', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.', '.', 'O'],
            ['.', '.', '.', '.', 'O', '.', '.', '.', '.'],
            ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
            ['R', '.', '.', '.', '.', '.', '.', 'B', 'R'],
            ['.', '.', 'C', '.', '.', '.', '.', 'Y', 'O']])

(def clicks [[4, 8], [2, 1], [4, 4], [6, 4], [4, 8], [1, 2], [1, 4], [4, 8], [6, 4]])

(def new-balls ['R', 'V', 'C', 'G', 'Y', 'O'])

(def new-balls-coordinates [[1, 2], [8, 5], [8, 6], [1, 1], [1, 8], [7, 4]])


(defn click-ball? [field coordenada]
  (not (= (get-in field coordenada) '.')))

(defn empty-cell [field coordenada]
  (assoc-in field coordenada '.'))

(defn fill-cell [field coordenada ball]
  (assoc-in field coordenada ball))

(defn make-move [field origen destino ball]
  (fill-cell (empty-cell field origen) destino ball))

(defn check-horizontal [field coordenada]
  (let [row (get field (first coordenada))
        ball (get-in field coordenada)]
    (filter )))


(defn recursividad [field or-des n-balls new-balls-c]
  (if (empty? or-des)
    "Terminado"
    (let [origen-valido (click-ball? field (first (first or-des)))
          destino-valido (not (click-ball? field(second (first or-des))))]
      (cond
        (and origen-valido (not destino-valido)) (recur field (rest or-des) n-balls new-balls-c)
        (and (not origen-valido) (not destino-valido)) (recur field (rest (rest or-des)) n-balls new-balls-c)
        :else 10))))


(defn linesGame [field clicks newBalls newBallsCoordinates]
  (let [origen-destino (zip clicks (rest clicks))]
    (recursividad field origen-destino new-balls new-balls-coordinates)))


(click-ball? field (clicks 1))
(empty-cell field [0 1])
(fill-cell field [0 1] 'R')
(make-move field (first clicks) (second clicks) 'R')
(check-horizontal field (clicks 1))


(linesGame field clicks new-balls new-balls-coordinates)





