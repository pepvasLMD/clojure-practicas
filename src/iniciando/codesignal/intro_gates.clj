(ns iniciando.codesignal.intro-gates)


(defn addTwoDigits [n]
  (loop [n n sum 0]
    (if (= n 0)
      sum
      (recur (int (/ n 10)) (+ sum (mod n 10))))))

(addTwoDigits 29)


(defn largestNumber [n]
  (- (int (Math/pow 10 n)) 1))