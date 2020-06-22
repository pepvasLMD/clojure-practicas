(ns iniciando.draw-rectangle)


(def canvas  [["a","a","a","a","a","a","a","a"],
              ["a","a","a","a","a","a","a","a"],
              ["a","a","a","a","a","a","a","a"],
              ["b","b","b","b","b","b","b","b"],
              ["b","b","b","b","b","b","b","b"]])

(defn r-r [c xi xf y]
  (if (< xi xf) (recur (assoc-in c [y xi] "-") (inc xi) xf y) c))

(defn r-c [c yi yf x]
  (if (< yi yf) (recur (assoc-in c [yi x] "|") (inc yi) yf x) c))

(defn drawRectangle [c [xi yi xf yf]]
  (let [t  (r-r c (inc xi) xf yi)
        b (r-r t (inc xi) xf yf)
        l (r-c b (inc yi) yf xi)
        r (r-c l (inc yi) yf xf)]
    (reduce (fn [x y] (assoc-in x y "*")) r [[yi xi] [yi xf] [yf xi] [yf xf]])))

(drawRectangle canvas [1 1 4 3])
