(ns iniciando.reacomodamiento-string-test
  (:require [clojure.test :refer :all]
            [iniciando.reacomodamiento-string :refer :all]))


(deftest contar
  (testing "Conto mal"
    (is (= (count (rotaciones ["aba" "abb" "abc"])) 123))))