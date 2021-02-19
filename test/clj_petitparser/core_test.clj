(ns clj-petitparser.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clj-petitparser.core :as pp]
            [clj-petitparser.input-stream :as in]
            [clj-petitparser.token :as t]))

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

(deftest repeating-parser-star
  (let [pp (pp/star "foo")]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (empty? (pp/parse pp "")))))

(deftest repeating-parser-plus
  (let [pp (pp/plus "foo")]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "bar")))))

(deftest repeating-parser-times
  (let [pp (pp/times "foo" 3)]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "foofoo")))))

(deftest repeating-parser-min
  (let [pp (pp/end (pp/min "foo" 3))]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (= ["foo" "foo" "foo" "foo"] (pp/parse pp "foofoofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "foofoo")))))

(deftest repeating-parser-max
  (let [pp (pp/end (pp/max "foo" 3))]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (= ["foo" "foo"] (pp/parse pp "foofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"End of input expected"
                          (pp/parse pp "foofoofoofoo")))))

(deftest not-parser
  (let [pp (pp/not \a)]
    (is (nil? (pp/parse pp "b")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #".+"
                          (pp/parse pp "a")))))

(deftest not-parser-does-not-consume-the-stream
  (let [stream (in/make-stream "b")
        pp (pp/not \a)
        result (pp/parse-on pp stream)]
    (is (zero? (in/position stream)))))

(deftest optional-parser
  (let [pp (pp/optional "foo")]
    (is (= "foo" (pp/parse pp "foo")))
    (is (nil? (pp/parse pp "bar")))))

(deftest token-parser
  (let [pp (pp/token "foo")
        token (pp/parse pp "foo")]
    (is (= 0 (t/start token)))
    (is (= 3 (t/count token)))
    (is (= "foo" (t/parsed-value token)))
    (is (= "foo" (t/input-value token)))))

(deftest action-parser-with-one-arg
  (let [pp (pp/transform (pp/token "foo")
                         (fn [token] (str/upper-case (t/input-value token))))]
    (is (= "FOO" (pp/parse pp "foo")))))

(deftest action-parser-with-n-args
  (let [pp (pp/transform [(pp/token "foo")
                          (pp/token "bar")
                          (pp/token "baz")]
                         (fn [[t1 t2 t3]]
                           (format "%s -> %s -> %s"
                                   (t/input-value t1)
                                   (t/input-value t2)
                                   (t/input-value t3))))]
    (is (= "foo -> bar -> baz" (pp/parse pp "foobarbaz")))))

(comment
 (re-find #"Literal '\s' expected" "Literal 'a' expected")
 (re-find #"Literal '" "Literal 'a' expected")
 (= (seq [\a \b \c]) [\a \b \c])
 ,)
