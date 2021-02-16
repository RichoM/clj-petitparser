(ns clj-petitparser.core-test
  (:require [clojure.test :refer :all]
            [clj-petitparser.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest literal-object-parser
  (let [pp (parser \a)]
    (is (= \a (parse pp "a")))))
