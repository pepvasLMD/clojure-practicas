(ns iniciando.codesignal.bomber)


(def field [["0", "0", "E", "0"],
            ["W", "0", "W", "E"],
            ["0", "E", "0", "W"],
            ["0", "W", "0", "E"]])

(defn get-col [field n]
  (reduce #(conj %1 (%2 n)) [] field))

(defn get-row [field n]
  (get field n))

(defn particion [coll n]
  [(take n coll) (drop n coll)])

(defn num-enemies [coll]
  (count (filter #(= "E" %) (take-while #(or (= "0" %) (= "E" %)) coll))))

(defn enemies-destroyer [coll x]
  (let [dividir (particion coll x)
        p1 (num-enemies (reverse (first dividir)))
        p2 (num-enemies (second dividir))]
    (+ p1 p2)))

(defn get-empty-cells [field]
  (filter #(not (nil? %)) (for [x (range (count (first field))) y (range (count field))]
                            (if (= (get-in field [y x]) "0") [x y]))))

(defn bomber [field]
  (let [empty-cells (get-empty-cells field)
        enemies-destroy-coll (map (fn [[x y]]
                                    (+ (enemies-destroyer (get-row field y) x) (enemies-destroyer (get-col field x) y)))
                                  empty-cells)]
    (if (empty? enemies-destroy-coll) 0 (apply max enemies-destroy-coll))))

(apply max (map (fn [[x y]] (+ x y)) (get-empty-cells field)))

(bomber field)


(defn checkIncreasingSequence [seq]
  (let [secuencia (count (filter (fn [[x y]] (>= x y)) (map vector seq (rest seq))))]
    (if (> secuencia 0 ) false true)))

(checkIncreasingSequence [1 2 3])







