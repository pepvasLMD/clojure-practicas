(ns iniciando.codesignal.intro-gates)


(defn addTwoDigits [n]
  (loop [n n sum 0]
    (if (= n 0)
      sum
      (recur (int (/ n 10)) (+ sum (mod n 10))))))

(addTwoDigits 29)


(defn largestNumber [n]
  (- (int (Math/pow 10 n)) 1))



(defn maxMultiple [divisor bound]
  (loop [d divisor b bound]
    (cond
      (= b 1) b
      (= (/ b d) (int (/ b d))) b
      :else (recur d (dec b)))))


(maxMultiple 3 10)


(defn circleOfNumbers [n f]
  (if (> (/ n 2) f) (+ f (/ n 2)) (- f (/ n 2))))

(circleOfNumbers 10 8)

(defn lateRide [n]
  (let [h (int (/ n 60)) m (mod n 60)]
    (+ (+ (int (/ h 10)) (mod h 10)) (+ (int (/ m 10)) (mod m 10)))))

(lateRide 240)


(defn phoneCall [min1 min2_10 min11 s]
  (loop [s s min 0]
    (cond
      (<= s 0) min
      (> min 9) (recur (- s min11) (inc min))
      (> min 0) (recur (- s min2_10) (inc min))
      :else (recur (- s min1) (inc min)))))

(phoneCall 10 1 2 22)


(defn chocan? [[y x] [y2 x2]]
  (let [diferencia-y (max (- y y2) (- y2 y))
        diferencia-x (max (- x x2) (- x2 x))]
    (cond
      (= diferencia-x diferencia-y) true
      (or (= x x2) (= y y2))  true
      (and (< x x2) (= y2 (+ y diferencia-x))) true
      (and (< x2 x) (= y (+ y2 diferencia-x))) true
      :else false)))


(defn squarequeen [q queries]
  (map #(chocan? q %) queries))


(defn squaresUnderQueenAttack [n queens queries]
  (let [a (map #(squarequeen % queries) queens)
        b (reduce (fn [x y] (map (fn [[x y]] (or x y)) (map vector x y))) (first a) (rest a))]
    (if (nil? b) [false] b)))


(squaresUnderQueenAttack 5 [[1 1] [3 2]] [[1 1] [0 3] [0 4] [3 4] [2 0] [4 3] [4 0]])

(squarequeen [1 1] [[1 1] [0 3] [0 4] [3 4] [2 0] [4 3] [4 0]])