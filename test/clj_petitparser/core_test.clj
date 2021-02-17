(ns clj-petitparser.core-test
  (:require [clojure.test :refer :all]
            [clj-petitparser.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest literal-object-parser
  (let [pp (as-parser \a)]
    (is (= \a (parse pp "a")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w' expected"
                          (parse pp "b")))))

(deftest literal-sequence-parser
  (let [pp (as-parser "abc")]
    (is (= "abc" (parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (parse pp "abd")))))

(deftest foo
  (let [pp (as-parser [\a \b \c])]
    (is (= [\a \b \c] (parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (parse pp "abd")))))

(comment
 (re-find #"Literal '\s' expected" "Literal 'a' expected")
 (re-find #"Literal '" "Literal 'a' expected")
 (= (seq [\a \b \c]) [\a \b \c])
 ,)
