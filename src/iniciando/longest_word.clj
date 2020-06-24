(ns iniciando.longest-word)

(defn longestWord
  ([t] (longestWord t "" ""))
  ([t curr ans]
   (cond
     (empty? t) (if (> (count curr) (count ans)) curr ans)
     (Character/isLetter (first t)) (recur (rest t) (str curr (first t)) ans)
     (> (count curr) (count ans)) (recur (rest t) "" curr)
     :else (recur (rest t) "" ans))))

(longestWord "ABCd" )



(defn validTime [t]
  (let [e (map #(Integer/parseInt %) (clojure.string/split t #":"))
        h (first e) m (second e)]
    (and (< h 24) (>= h 0) (> 60 m) (>= m 0))))


(validTime "13:58")


(defn sumUpNumbers [inputString]
  (reduce + (map read-string (re-seq #"\d+" inputString))))

(sumUpNumbers "2 apples, 12 oranges")



(defn get-cuadrado [matrix x y n]
  (reduce (fn [cuadrado row] (conj cuadrado (take n (drop x row)))) [] (take n (drop y matrix))))

(defn differentSquares [matrix]
  (count (set (for [x (range (- (count (first matrix)) 1))
             y (range (- (count matrix) 1))]
         (get-cuadrado matrix x y 2)))))

(differentSquares [[1, 2, 1],
                   [2, 2, 2],
                   [2, 2, 2],
                   [1, 2, 3],
                   [2, 2, 1]])


(defn product-digits [num]
  (loop [n num acc 1]
    (if (> n 0) (recur (int (/ n 10)) (* acc (mod n 10))) acc)))

(defn digitsProduct [product]
  (loop [n 11]
    (if (> product (product-digits n)) (recur (inc n)) n)))

(= 450 (product-digits 2559))
(digitsProduct 450)

(def work (atom 459))
(def i 7)

(defn digitsProduct2 [product]
  (or (->> (range 1 10000)
           (map (fn [x] [(apply * (map (comp read-string str) (str x))) x]))
           (drop-while (comp (complement #{product}) first))
           first last) -1))


(digitsProduct2 450)



(defn new-name [name names]
  (if (< (.indexOf names name) 0)
    name
    (loop [n 1]
      (if (>= (.indexOf names (str name "(" n ")")) 0)
        (recur (inc n))
        (str name "(" n ")")))))

(defn fileNaming [names]
  (reduce #(conj %1 (new-name %2 %1)) [] names))

(fileNaming ["doc" "doc" "image" "doc(1)" "doc"])

(new-name "doc" ["doc" "image" "doc(1)" "doc"])



(defn messageFromBinaryCode [code]
  (apply str (map (comp char (fn [x] (Integer/parseInt (apply str x) 2))) (partition 8 code))))

(messageFromBinaryCode "010010000110010101101100011011000110111100100001")



(defn spiralNumbers [num]
  (let [mayor (* num num)]
    (loop [y 0 x 0 n 1 m (vec (repeat num (vec (repeat num 0)))) d :right]
      (cond
        (> n mayor) m
        (= (get-in m [y x]) 0) (recur y x (inc n) (assoc-in m [y x] n) d)
        (and (= d :right) (nil? (get-in m [y (inc x)]))) (recur y x n m :down)
        (and (= d :right) (> (get-in m [y (inc x)]) 0)) (recur y x n m :down)
        (= d :right) (recur y (inc x) n m d)
        (and (= d :down) (nil? (get-in m [(inc y) x]))) (recur y x n m :left)
        (and (= d :down) (> (get-in m [(inc y) x]) 0)) (recur y x n m :left)
        (= d :down) (recur (inc y) x n m d)
        (and (= d :left) (nil? (get-in m [y (dec x)]))) (recur y x n m :up)
        (and (= d :left) (> (get-in m [y (dec x)]) 0)) (recur y x n m :up)
        (= d :left) (recur y (dec x) n m d)
        (and (= d :up) (nil? (get-in m [(dec y) x]))) (recur y x n m :right)
        (and (= d :up) (> (get-in m [(dec y) x]) 0)) (recur y x n m :right)
        (= d :up) (recur (dec y) x n m d)
        :else m))))

(spiralNumbers 100)

(def grid [[1, 3, 2, 5, 4, 6, 9, 8, 7],
           [4, 6, 5, 8, 7, 9, 3, 2, 1],
           [7, 9, 8, 2, 1, 3, 6, 5, 4],
           [9, 2, 1, 4, 3, 5, 8, 7, 6],
           [3, 5, 4, 7, 6, 8, 2, 1, 9],
           [6, 8, 7, 1, 9, 2, 5, 4, 3],
           [5, 7, 6, 9, 8, 1, 4, 3, 2],
           [2, 4, 3, 6, 5, 7, 1, 9, 8],
           [8, 1, 9, 3, 2, 4, 7, 6, 5]])

(defn get-col [field n]
  (reduce #(conj %1 (%2 n)) [] field))

(defn get-row [field n]
  (get field n))


(defn sudoku [grid]
  (let [check-row-col (some #(not (= 9 (count (set (get-row grid %))) (count (set (get-col grid %))))) (range 9))
        cuadrados (map (fn [[x y n]] (get-cuadrado grid x y n)) (for [x (range 3) y (range 3)] [(* 3 x) (* 3 y) 3]))
        check-cuadrados (some #(not (= 9 (count (set %)))) (map (fn [x] (into [] cat x)) cuadrados))]
    (if (nil? check-row-col) (nil? check-cuadrados) false)))

(sudoku grid)

