(ns iniciando.arbol)

(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t)
                  :L (xconj (:L t) v)
                  :R (:R t)}
    :else {:val (:val t),
           :L (:L t),
           :R (xconj (:R t) v)}))

(def tree1 (xconj nil 5))

(def tree1 (xconj tree1 3))

(def tree1 (xconj tree1 2))


(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))


(xseq tree1)


(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))


(rec-step [1 2 3 4])

(defn lz-rec-step [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lz-rec-step (rest s))]
      [])))

(lz-rec-step [1 2 3 4])

;=> (1 (2 (3 (4 ()))))

(class (lz-rec-step [1 2 3 4]))
;=> clojure.lang.LazySeq

(dorun (lz-rec-step (range 200000)))


(defn pi
  "Approximate Pi to the 1/n decimal with Leibniz formula"
  [n]
  (transduce
    (comp (map #(/ 4 %)) (take n))
    +
    (iterate #(* ((if (pos? %) + -) % 2) -1) 1.0)))