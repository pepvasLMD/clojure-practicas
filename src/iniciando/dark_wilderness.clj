(ns iniciando.dark-wilderness)

(defn growingPlant
  ([u d dH] (growingPlant u d dH 1))
  ([u d dH acc]
   (if (> (- dH u) 0)
     (recur u d (- dH (- u d)) (inc acc)) acc)))

(growingPlant 100 10 910)


(defn knapsackLight [value1 weight1 value2 weight2 maxW]
  (cond
    (>= maxW (+ weight2 weight1)) (+ value1 value2)
    (and (>= maxW weight1) (> value1 value2)) value1
    (and (>= maxW weight2) (> value2 value1)) value2
    (>= maxW weight1) value1
    (>= maxW weight2) value2
    :else 0))

(knapsackLight 10 2 11 3 1)


(defn longestDigitsPrefix [inputString]
  (let [a (re-find #"\d+" inputString)]
    (if a (if (= a (apply str(take (count a) inputString))) a "") "") ))


(defn digitDegree
  ([n] (digitDegree n 0))
  ([n acc] (if (> 10 n) acc (recur (apply + (map #(- (int %) 48) (str n))) (inc acc)))))


(defn chocan? [[c1 n1] [c2 n2] acc]
  (if (> c2 c1)
    (recur [(inc c1) (+ acc n1)] [c2 n2] acc)
    (and (= n2 n1) (= c2 c1))))

(defn bishopAndPawn [[c1 n1] [c2 n2]]
  (let [ic1 (int c1) in1 (int n1) ic2 (int c2) in2 (int n2)]
    (= true (some (fn [[c1 n1 c2 n2 acc]] (chocan? [c1 n1] [c2 n2] acc))
          [[ic1 in1 ic2 in2 1] [ic2 in2 ic1 in1 1] [ic1 in1 ic2 in2 -1] [ic2 in2 ic1 in1 -1]]))))

(bishopAndPawn "a5" "c3")