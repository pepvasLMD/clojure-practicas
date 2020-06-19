(ns iniciando.tetris
  (
    :import [javax.swing JFrame JLabel JButton]
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


;(.show (javax.swing.JFrame.))
