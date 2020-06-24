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



(defn spiralNumbers [n]
  (let [matrix (repeat n (repeat n 0))]
    (loop [y 0 x 0 n 1 matrix (vec (repeat 5 (vec (repeat 5 0))))]
      (cond
        (if (> (get-in matrix [y x]) 0) (recur y x (inc n) (assoc-in matrix [y x] n)))))))

(spiralNumbers 5)