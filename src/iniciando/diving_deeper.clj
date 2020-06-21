(ns iniciando.diving-deeper)



(defn extractEachKth [a k]
  (into [] cat (partition-all (dec k) k a)))

(extractEachKth [1 2 3 4 5 6 7 8 9 10] 3)

(def digito #"\d")


(defn differentSymbolsNaive [s]
  (count (reduce (fn [x y] (if (some #(= % y) x) x (conj x y))) [] s)))

(differentSymbolsNaive "cabca")

(defn arrayMaxConsecutiveSum [inputArray k]
  (loop [i k sum (apply + (take k inputArray)) maxS sum]
    (if (< i (count inputArray))
      (let [nSum (+ (inputArray i) (- sum (inputArray (- i k))))]
        (recur (inc i) nSum (if (> nSum maxS) nSum maxS)))
      maxS)))



(time (arrayMaxConsecutiveSum (vec (range 1000000)) 2))