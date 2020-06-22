(ns iniciando.eruption-of-light)


(defn recursivo [cadena contador caracter]
  (let [a (filter #(= caracter %) cadena)]
    (if (empty? a)
      (empty? cadena)
      (if (> (count a) contador) false (recur (filter #(not (= caracter %)) cadena) (count a) (char (inc (int caracter))))))))

(defn isBeautifulString [inputString]
  (let [a (filter #(= % \a) inputString)]
    (recursivo inputString (count a) \a)))


(defn findEmailDomain [address]
  (last (clojure.string/split address #"\@")))

(findEmailDomain "John.Smith@example.com")


(defn palindromoV2 [subcadena cadena]
  (let [pal (str subcadena (apply str (reverse (butlast subcadena))))
        pal2 (str subcadena (apply str (reverse subcadena)))]
    (cond
      (.contains pal cadena) pal
      (.contains pal2 cadena) pal2
      (= subcadena cadena) (str subcadena (apply str (reverse subcadena)))
      :else (recur (str subcadena (get cadena (count subcadena))) cadena))))

(defn buildPalindrome [st]
  (let [mitad (/ (count st) 2)]
    (if (= st (apply str (reverse st)))
      st
      (palindromoV2 (apply str (take mitad st)) st))))

(buildPalindrome "abc")



(defn electionsWinners [votes k]
  (let [maximo (apply max votes)
        iguales (count (filter #(= % maximo) votes))]
    (cond
      (and (= k 0) (> iguales 1)) 0
      (= k 0) 1
      :else (count (filter #(> (+ k %) maximo ) votes)))))


(electionsWinners [2 3 5 2] 3)


(defn isMAC48Address [inputString]
  (not (nil? (re-find #"^([0-9A-F]{2}-){5}[0-9A-F]{2}$" inputString))))

(isMAC48Address "00-1B-63-84-45-E22")