(ns iniciando.tetris
  (:import [javax.swing JFrame JLabel JButton]
           [java.awt.event WindowListener]))


;code by https://shaunlebron.github.io/t3tr0s-slides/

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(def pieces
  {:I [ [-1  0] [ 0  0] [ 1  0] [ 2  0] ]
   :T [ [ 0 -1] [-1  0] [ 0  0] [ 1  0] ]
   :O [ [ 0 -1] [ 1 -1] [ 0  0] [ 1  0] ]
   :J [ [-1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :L [ [ 1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :S [ [ 0 -1] [ 1 -1] [-1  0] [ 0  0] ]
   :Z [ [-1 -1] [ 0 -1] [ 0  0] [ 1  0] ]})


(get pieces :I)



(def initial-state {:board empty-board
                    :piece nil
                    :position nil})


(assoc initial-state :piece (:J pieces)
                     :position [4 6])

(let [a (assoc initial-state :piece (:J pieces) :position [4 6])] (assoc a :piece "asdf"))

(defn rotate-cord [[x y]]
  [(- y) x])

(rotate-cord [4 6])


(defn rotate-piece [piece]
  (mapv rotate-cord piece))

(:J pieces)
(rotate-piece (:J pieces))

(defn move-left  [[x y]] [ (dec x) y ])
(defn move-right [[x y]] [ (inc x) y ])
(defn move-down  [[x y]] [ x (inc y) ])

(def g0  {:position [4 6] :piece (:J pieces)})

(def g1  (update-in g0  [:position] move-left))
(def g2  (update-in g1  [:position] move-left))
(def g3  (update-in g2  [:piece]    rotate-piece))
(def g4  (update-in g3  [:position] move-down))
(def g5  (update-in g4  [:position] move-down))
(def g6  (update-in g5  [:piece]    rotate-piece))
(def g7  (update-in g6  [:position] move-right))
(def g8  (update-in g7  [:position] move-right))
(def g9  (update-in g8  [:piece]    rotate-piece))
(def g10 (update-in g9  [:position] move-down))
(def g11 (update-in g10 [:position] move-down))
(def g12 (update-in g11 [:position] move-down))
(def g13 (update-in g12 [:position] move-down))
(def g14 (update-in g13 [:position] move-down))
(def g15 (update-in g14 [:position] move-down))

(def game
  (atom {:board empty-board
         :piece (:J pieces)
         :position [4 6]}))



(defn my-zip [x y]
  (map vector x y))

(defn get-interest [x y]
  (+ x (* x (/ y 100))))


(defn sum-interest [x]
  (let [intereses (map #(get-interest (first %1) (second %1)) x)]
    (reduce + (first intereses) (rest intereses))))

(defn combinaciones [x]
  (rest (reduce (fn [x y]
                  (conj x (conj (butlast (last x)) (last (last x))))) [x] x)))


(combinaciones [1 2 3])

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))



(defn reduce-debt [debts pay]
  (reduce (fn [x y]
            (if (>= (-(second x) y) 0)
              [(conj (first x) 0) (-(second x) y)]
              [(conj (first x) (- y (second x))) 0])) [[] pay] debts))

(reduce-debt [2 2 5] 15)


(defn coverDebts [salary debts interests]
  (let [max-pay (/ salary 10)
        zip (my-zip debts interests)
        comb (combinaciones zip)
        perm (permutations debts)
        ]
     (map (fn [x] (reduce-debt x max-pay)) perm)))


(coverDebts 50 [2 2 5] [200 100 150])

(map (fn [x] (get-interest (first x) (second x))) [[2 200] [2 100] [5 150]])

(sum-interest (my-zip [2 2 5] [200 100 150]) )


(permutations [1 2 3])


(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])



(defn prep [board]
  (map #(partition 3 %)
       (partition 9 board)))




(defn print-board [board]
  (let [row-sep (apply str (repeat 37 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (print "| ")
      (doseq [subrow (nth board row)]
        (doseq [cell (butlast subrow)]
          (print (str cell " ")))
        (print (str (last subrow) " | ")))
      (println)
      (when (zero? (mod (inc row) 3))
        (println row-sep)))))



(defn rows [board sz]
  (partition sz board))


(defn row-for [board index sz]
  (nth (rows board sz) (/ index 9)))



(defn column-for [board index sz]
  (let [col (mod index sz)]
    (map #(nth % col)
         (rows board sz))))


(defn subgrid-for [board i]
  (let [rows (rows board 9)
        sgcol (/ (mod i 9) 3)
        sgrow (/ (/ i 9) 3)
        grp-col (column-for (mapcat #(partition 3 %) rows) sgcol 3)
        grp (take 3 (drop (* 3 (int sgrow)) grp-col))]
    (flatten grp)))


(defn numbers-present-for [board i]
  (set
    (concat (row-for board i 9)
            (column-for board i 9)
            (subgrid-for board i))))


(-> b1 prep print-board)


(row-for b1 1 9)


(subgrid-for b1 1)


(numbers-present-for b1 1)

(numbers-present-for (assoc b1 1 8) 1)

(set/difference #{1 2 3 4 5 6 7 8 9}
                (numbers-present-for b1 1))


;(.show (javax.swing.JFrame.))
