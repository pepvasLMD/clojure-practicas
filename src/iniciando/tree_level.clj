(ns iniciando.tree-level)

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