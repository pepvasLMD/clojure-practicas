(ns iniciando.joy-clojure.chess)


(defn initial-board []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])


(def ^:dynamic *file-key* \a)
(def ^:dynamic *rank-key* \0)

(defn- file-component [file]
  (- (int file) (int *file-key*)))


(defn- rank-component [rank]
  (->> (int *rank-key*)
       (- (int rank))
       (- 8)
       (* 8)))

(defn- index [file rank]
  (+ (file-component file) (rank-component rank)))

(defn lookup [board pos]
  (let [[file rank] pos]
    (board (index file rank))))

(letfn [(index [file rank]
          (let [f (- (int file) (int \a))
                r (* 8 (- 8 (- (int rank) (int \0))))]
            (+ f r)))]

  (defn lookup2 [board pos]
    (let [[file rank] pos]
      (board (index file rank)))))



(defn lookup3 [board pos]
  (let [[file rank] (map int pos)
        [fc rc] (map int [\a \0])
        f (- file fc)
        r (* 8 (- 8 (- rank rc)))
        index (+ f r)]
    (board index)))


(lookup (initial-board) "a1")
(lookup2 (initial-board) "a1")
(lookup3 (initial-board) "a1")


(defn make-set
  "Takes two values and makes a set from them."
  [x y]
  (println "Making a set")
  #{x y})

(defn make-set
  ([x] #{x})
  ([x y] #{x y}))


(make-set 11)


(defn arity2+ [first second & more]
  (vector first second more))


(def make-list0 #(list))

(make-list0)


(def make-list2 #(list %1 %2))

(make-list2 1 2)


(def make-list2+ #(list %1 %2 %&))

(make-list2+ 1 2 3 4 4 5 6)


(do
  (def x 5)
  (def y 4)
  (+ x y)
  [x y])


(let [r 5
      pi 3.1415
      r-squared (* r r)]
  (println "radius is" r)
  (* pi r-squared))


(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(print-down-from 10)


(defn conj-down [x]
  (when (not-empty x)
    x
    (recur (rest x))))

(conj-down [1 2 3 4])



