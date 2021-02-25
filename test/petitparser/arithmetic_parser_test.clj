(ns petitparser.arithmetic-parser-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [petitparser.core :as pp]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :as r]))

(def grammar
  {:start :number
   :number (pp/trim (pp/flatten [(pp/optional \-)
                                 (pp/plus pp/digit)
                                 (pp/optional [\.
                                               (pp/plus pp/digit)])])
                    pp/space)})

(def transformations
  {:number (fn [value] (read-string value))})

(def pp (pp/compose grammar transformations))

(deftest num-test
  (is (= 0 (pp/parse pp "0")))
  (is (= 0.0 (pp/parse pp "0.0")))
  (is (= 1 (pp/parse pp "1")))
  (is (= 1.2 (pp/parse pp "1.2")))
  (is (= 34 (pp/parse pp "34")))
  (is (= 56.78 (pp/parse pp "56.78")))
  (is (= -9 (pp/parse pp "-9")))
  (is (= -9.9 (pp/parse pp "-9.9"))))
