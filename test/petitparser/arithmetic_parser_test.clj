(ns petitparser.arithmetic-parser-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [petitparser.core :as pp]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :as r]))

(def grammar
  {:start (pp/end :terms)
   :terms (pp/or :addition :factors)
   :factors (pp/or :multiplication :power)
   :multiplication (pp/separated-by :power :multiplication-ops)
   :multiplication-ops (pp/trim (pp/or "*" "/") pp/space)
   :power (pp/separated-by :primary :power-ops)
   :power-ops (pp/trim "^" pp/space)
   :primary (pp/or :number :parentheses)
   :parentheses [(pp/trim "(" pp/space)
                 :terms
                 (pp/trim ")" pp/space)]
   :addition (pp/separated-by :factors :addition-ops)
   :addition-ops (pp/trim (pp/or "+" "-") pp/space)
   :number (pp/trim (pp/flatten [(pp/optional \-)
                                 (pp/plus pp/digit)
                                 (pp/optional [\. (pp/plus pp/digit)])])
                    pp/space)})

(def operations {"+" +, "-" -, "*" *, "/" /,
                 "^" (fn [a b] (Math/pow b a))})

(defn reduce-operands [nodes]
  (let [total (first nodes)
        pairs (partition 2 (next nodes))]
    (reduce (fn [sub [op n]] (op sub n)) total pairs)))

(def transformations
  {:number (comp double read-string)
   :parentheses second
   :addition reduce-operands
   :multiplication reduce-operands
   :power (comp reduce-operands reverse)
   :addition-ops operations
   :multiplication-ops operations
   :power-ops operations})

(def pp (pp/compose grammar transformations))

(deftest number-test
  (is (= 0.0 (pp/parse pp "0")))
  (is (= 0.0 (pp/parse pp "0.0")))
  (is (= 1.0 (pp/parse pp "1")))
  (is (= 1.2 (pp/parse pp "1.2")))
  (is (= 34.0 (pp/parse pp "34")))
  (is (= 56.78 (pp/parse pp "56.78")))
  (is (= -9.0 (pp/parse pp "-9")))
  (is (= -9.9 (pp/parse pp "-9.9"))))

(deftest addition-test
  (is (= 3.0 (pp/parse pp "1 + 2")))
  (is (= 3.0 (pp/parse pp "2 + 1")))
  (is (= 3.3 (pp/parse pp "1 + 2.3")))
  (is (= 3.3 (pp/parse pp "2.3 + 1")))
  (is (= -1.0 (pp/parse pp "1 + -2")))
  (is (= -1.0 (pp/parse pp "-2 + 1")))
  (is (= 6.0 (pp/parse pp "1 + 2 + 3")))
  (is (= 10.0 (pp/parse pp "1 + 2 + 3 + 4")))
  (is (= 15.0 (pp/parse pp "1 + 2 + 3 + 4 + 5"))))

(deftest division-test
  (is (= 4.0 (pp/parse pp "12 / 3")))
  (is (= 4.0 (pp/parse pp "-16 / -4")))
  (is (= 25.0 (pp/parse pp "100 / 2 / 2")))
  (is (= 5.0 (pp/parse pp "100 / 2 / 2 / 5")))
  (is (= 1.0 (pp/parse pp "100 / 2 / 2 / 5 / 5"))))

(deftest multiplication-test
  (is (= 6.0 (pp/parse pp "2 * 3")))
  (is (= -8.0 (pp/parse pp "2 * -4")))
  (is (= 6.0 (pp/parse pp "1 * 2 * 3")))
  (is (= 24.0 (pp/parse pp "1 * 2 * 3 * 4")))
  (is (= 120.0 (pp/parse pp "1 * 2 * 3 * 4 * 5"))))

(deftest pow-test
  (is (= 8.0 (pp/parse pp "2 ^ 3")))
  (is (= -8.0 (pp/parse pp "-2 ^ 3")))
  (is (= -0.125 (pp/parse pp "-2 ^ -3")))
  (is (= 64.0 (pp/parse pp "4 ^ 3")))
  (is (= 262144.0 (pp/parse pp "4 ^ 3 ^ 2")))
  (is (= 262144.0 (pp/parse pp "4 ^ 3 ^ 2 ^ 1")))
  (is (= 262144.0 (pp/parse pp "4 ^ 3 ^ 2 ^ 1 ^ 0"))))

(deftest subtraction-test
  (is (= -1.0 (pp/parse pp "1 - 2")))
  (is (= 0.0 (pp/parse pp "1.2 - 1.2")))
  (is (= 3.0 (pp/parse pp "1 - -2")))
  (is (= 1.0 (pp/parse pp "-1 - -2")))
  (is (= -4.0 (pp/parse pp "1 - 2 - 3")))
  (is (= -8.0 (pp/parse pp "1 - 2 - 3 - 4")))
  (is (= -13.0 (pp/parse pp "1 - 2 - 3 - 4 - 5"))))

(deftest priority-test
  (is (= 10.0 (pp/parse pp "2 * 3 + 4")))
  (is (= 14.0 (pp/parse pp "2 + 3 * 4")))
  (is (= 6.0 (pp/parse pp "6 / 3 + 4")))
  (is (= 5.0 (pp/parse pp "2 + 6 / 2"))))

(deftest parens-test
  (is (= 1.0 (pp/parse pp "(1)")))
  (is (= 3.0 (pp/parse pp "(1 + 2)")))
  (is (= 1.0 (pp/parse pp "((1))")))
  (is (= 3.0 (pp/parse pp "((1 + 2))")))
  (is (= 14.0 (pp/parse pp "2 * (3 + 4)")))
  (is (= 20.0 (pp/parse pp "(2 + 3) * 4")))
  (is (= 1.0 (pp/parse pp "6 / (2 + 4)")))
  (is (= 4.0 (pp/parse pp "(2 + 6) / 2"))))
