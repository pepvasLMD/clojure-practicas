(ns iniciando.codesignal.simplify-path
  (:require [clojure.string :as string]))

(def path "/home/a/./x/../b//c/")

(defn parent-directory [path]
  (re-find (re-pattern "\\d+") "asdf321asfd") )


(defn simplifyPath [path]
  path)

(simplifyPath path)

(string/replace "The color is red" #"red" "blue")

(parent-directory path)
