(ns iniciando.tree-level)

(def binary-tree "(0(5(6()())(14()(9()())))(7(1()())(23()())))")

(defstruct BST :val :left :right)

(struct BST 1 )


(def arbol (struct BST 1))

(defn insertarL [arbol val l r]
  (let [izq (struct BST val l r)]
    (assoc arbol :left izq)))

(insertarL arbol 10 nil nil)