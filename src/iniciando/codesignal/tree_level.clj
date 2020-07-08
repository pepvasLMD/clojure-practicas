(ns iniciando.codesignal.tree-level)

(def binary-tree "(0(5(6()())(14()(9()())))(7(1()())(23()())))")

(defstruct BST :val :left :right)

(struct BST 1 )

(defn insertarL 
  ([arbol val] (insertarL arbol val nil nil))
  ([arbol val l] (insertarL arbol val l nil))
  ([arbol val l r]
   (let [izq (struct BST val l r)]
     (assoc arbol :left izq))))

(def arbol (struct BST 1))


(insertarL arbol 10 )

(defn recursivo [min1 min2_10 min11 s mins]
  (cond
    (= s 0) mins
    (< s 0) (dec mins)
    (= mins 0) (recur min1 min2_10 min11 (- s min1) (inc mins))
    (< mins 10) (recur min1 min2_10 min11 (- s min2_10) (inc mins))
    :else (int (+ mins (/ s min11)))))


(defn phoneCall [min1 min2_10 min11 s]
  (recursivo min1 min2_10 min11 s 0))

(phoneCall 1 2 1 6)


(defn a [t k n]
  )


(defn treeLevelSum [t k])


(defn fareEstimator [ride_time ride_distance cost_per_minute cost_per_mile]
  (let [union (map vector cost_per_minute cost_per_mile)]
    (map (fn [[x y]] (+ (* x ride_time) (* y ride_distance))) union)))


(defn calcular-distancia [xi xf]
  (let [x1 (- xi (int xi))
        x2 (- xf (int xf))
        xT [ (+ (- 1 x1) (- 1 x2)) (+ x1 (- 1 x2)) (+ (- 1 x1) x2)]]
    [xT]))


(defn perfectCity [[xi yi] [xf yf]]
  (calcular-distancia xi xf))


(perfectCity [0.4 1] [0.9 3])