(ns iniciando.codesignal.reacomodamiento-string)

(defn rotations [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (mapcat
      (fn [[x & xs]] (map #(cons x %) (permutations xs)))
      (rotations a-set))))


(defn stringsRearrangement [s]
  (->> s
       permutations
       (some
         #(reduce
            (fn [acc itm]
              (if (= 1 (count (filter false? (map = acc itm))))
                itm (reduced false))) %))
       boolean))

(stringsRearrangement ["aba", "bbb", "bab"])



(defn
  ^{:doc "mymax [xs+] gets the maximum value in xs using > "
    :test (fn []
            (assert (= 42  (mymax 2 42 5 4))))
    :user/comment "this is the best fn ever!"}
  mymax
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce mymax (mymax x y) more)))



(defn climbingStaircase [n k]
  (permutations (range k)))


(climbingStaircase 4 2)

(defn addTwoDigits [n]
  (loop [n n sum 0 ]
    (cond
      (= n 0) sum
      :else (recur (int (/ n 10)) (+ sum (mod n 10))))))

(addTwoDigits 29)



(defn pay-debt [s debts new-debts]
  (cond
    (= s 0) (into new-debts debts)
    (>= s (first debts)) (recur (- s (first debts)) (rest debts) (conj new-debts 0))
    :else (into (conj new-debts (- (first debts) s)) debts)))


(defn r [a-seq]
  (let [a-vec (vec a-seq)]
    (for [i (range (count a-vec))]
      (concat (subvec a-vec i) (subvec a-vec 0 i)))))

(r [[1 2] [3 5] [8 9]])



(defn coverDebts [s debts interests]
  (loop [max-pay (int (* s 0.1)) debts debts acc 1]
    (let [pay-debt (pay-debt max-pay debts [])]
      (cond
        (= (apply + pay-debt) 0) acc
        (= acc 10) pay-debt
        :else (recur max-pay (map (fn [[d i]] (int (+ d (* d (/ i 100))))) (map vector pay-debt interests)) (inc acc))))))


(coverDebts 50 [2 2 5] [200 100 150])


(pay-debt 5 [2 2 5] [])

(map #(pay-debt 4 %1 []) (permutations [2 2 5]))

(map (fn [[d i]] (int (+ d (* d (/ i 100))))) (map vector [0 0 5] [200 100 150]))




(defn- bubble [ys x]
  (if-let [y (peek ys)]
    (if (> y x)
      (conj (pop ys) x y)
      (conj ys x))
    [x]))

(defn bubble-sort [xs]
  (let [ys (reduce bubble [] xs)]
    (if (= xs ys)
      xs
      (recur ys))))

(bubble-sort [3 2 1])



(def d (delay (println "Corriendo.... ") :done!))


