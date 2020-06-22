(ns iniciando.rainbow-of-clarity)

(defn isDigit [symbol]
  (and (>= (int symbol) 48) (> 58 (int symbol))))

(isDigit \9)


(defn encoding [n-s s]
  (if (empty? s)
    n-s
    (let [letras (take-while #(= % (first s)) s)
          s-n (if (< (count letras) 2) (apply str letras) (str (count letras) (first s)))]
      (recur (str n-s s-n) (drop-while  #(= % (first s)) s)))))

(defn lineEncoding [s]
  (encoding "" s))

(lineEncoding "aabbbc")

(re-find #"([a-z])1*" "aabbbc")

(clojure.string/replace "aabbbc" #"([a-z])1*" #(str (count %) (first %)))



(defn chessKnight [[l n]]
  (let [p [(int l) (int n)]
        m [[-2 1] [-1 2] [2 1] [1 2] [2 -1] [1 -2] [-2 -1] [-1 -2]]
        all-m (map #(map + % p) m)]
    (count (filter (fn [[l n]] (and (> l 96) (< l 105) (> n 48) (< n 57))) all-m))))

(chessKnight "a1")


(defn deleteDigit [n]
  (let [digitos (map-indexed (fn [i x] [i (int x)]) (str n))
        minimo (apply min digitos)]
    digitos))

(deleteDigit 123)