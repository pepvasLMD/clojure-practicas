(ns iniciando.codesignal.kingdom-roads)


(def cities 4)
(def roads [[0 1] [1 2] [2 0]])

(defn roadsBuilding [cities roads]
  (sort
    (for [x (range cities) y (range cities)
          :while (not= x y)
          :when (empty? (filter #(or (= [x y] %) (= [y x] %)) roads))]
      [(min x y) (max x y)])))

(roadsBuilding cities roads)


(defn efficientRoadNetwork [n roads]
  roads)



(efficientRoadNetwork 6 [[3, 0], [0, 4], [5, 0], [2, 1], [1, 4], [2, 3], [5, 2]])


(= 3 ((#(fn [a b] (% b a)) nth) 2 [1 2 3 4 5]))



((fn [x y]
   (let [val (mod x (count y))]
     (concat (drop val y) (take val y)))) 2 [1 2 3 4 5])

(defn get-col [field n]
  (reduce #(conj %1 (%2 n)) [] field))


((fn [x]
   (let [g-r (fn [y] (get x y))
         g-c (fn [y] (reduce #(conj %1 (%2 y)) [] x))
         c-r (some true? (map (fn [x] (every? #(= :x %) (g-r x))) [0 1 2]))]
     c-r))
  [[:e :e :e]
     [:e :e :e]
     [:e :e :e]])



(fn compress-sequence [coleccion]
  (reverse (reduce (fn [coleccion-final elemento]
                     (if (= (first coleccion-final) elemento)
                       coleccion-final
                       (cons elemento coleccion-final))) '() coleccion)))