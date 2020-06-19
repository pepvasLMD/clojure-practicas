(ns iniciando.reverseInParentheses)


(def parentesis-pattern #"\(\w*\)")

(defn reverseInParentheses [cadena]
  (if (re-find parentesis-pattern cadena)
    (recur (clojure.string/replace cadena parentesis-pattern #(apply str (rest (reverse (rest %1))))))
    cadena))


(re-matches #"hello, (.*)" "hello, world")

(re-find (re-matcher #"(.*)(\\d+)(.*)" "This order was placed for QT3000! OK?"))

(re-find (re-pattern "\\d+") "abc123def")



(def sample "20170426-17:20:04.005|bip.com|1.0.0|alert|Update,john,12")
(defrecord Logline [datetime action user id])
(def pattern #"(\d{8}-\d{2}:\d{2}:\d{2}.\d{3})\|.*\|(\w*),(\w*),(\d*)")
(defn parser [line]
  (if-let [[_ dt a u i] (re-find pattern line)]
    (->Logline dt a u i)))


(parser sample)

(reverseInParentheses "foo(bar)baz(blim)")
