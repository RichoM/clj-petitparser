(ns clj-petitparser.core-test
  (:require [clojure.test :refer :all]
            [clj-petitparser.core :as pp]
            [clj-petitparser.input-stream :as in]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest literal-object-parser
  (let [pp (pp/as-parser \a)]
    (is (= \a (pp/parse pp "a")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w' expected"
                          (pp/parse pp "b")))))

(deftest literal-sequence-parser
  (let [pp (pp/as-parser "abc")]
    (is (= "abc" (pp/parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "abd")))))

(deftest sequence-parser
  (let [pp (pp/as-parser [\a \b \c])]
    (is (= [\a \b \c] (pp/parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "abd")))))

(deftest choice-parser
  (let [pp (pp/or "perro"
                  "gato")]
    (is (= "perro" (pp/parse pp "perro")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "ratón")))))

(deftest flatten-parser
  (let [pp (pp/flatten [(pp/or "perro" "gato" "león")
                        (pp/or "_" " " "-" "/")
                        (pp/or "hambriento" "cansado" "feliz")])]
    (is (= "gato feliz" (pp/parse pp "gato feliz!!!")))))

(deftest and-parser
  (let [pp (pp/as-parser ["foo" (pp/and "bar")])
        stream (in/make-stream "foobar")
        result (pp/actual-result (pp/parse-on pp stream))]
    (is (= ["foo" "bar"] result))
    (is (= 3 (in/position stream)))))

(deftest end-parser
  (let [pp (pp/end "foo")]
  (is (= "foo" (pp/parse pp "foo")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"End of input expected"
                        (pp/parse pp "foobar")))))

(comment
 (re-find #"Literal '\s' expected" "Literal 'a' expected")
 (re-find #"Literal '" "Literal 'a' expected")
 (= (seq [\a \b \c]) [\a \b \c])
 ,)
