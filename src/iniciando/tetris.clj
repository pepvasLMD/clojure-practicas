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



(defn swing []
  (let [frame (JFrame. "Fund manager")
        label (JLabel. "Exit on close")]
    (doto frame
      (.add label)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.addWindowListener
        (proxy [WindowListener] []
          (windowClosing [evt]
            (println "Whoop"))))
      (.setVisible true))))

(swing)