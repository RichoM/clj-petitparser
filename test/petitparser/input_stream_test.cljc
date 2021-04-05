(ns petitparser.input-stream-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is]])
            [petitparser.input-stream :as in]))

(deftest next!-increments-position
  (let [stream (in/make-stream "abc")]
    (is (= 0 (in/position stream)))
    (is (= \a (in/next! stream)))
    (is (= 1 (in/position stream)))
    (is (= \b (in/next! stream)))
    (is (= 2 (in/position stream)))
    (is (= \c (in/next! stream)))
    (is (= 3 (in/position stream)))))

(deftest peek-does-not-increment-position
  (let [stream (in/make-stream "abc")]
    (is (= 0 (in/position stream)))
    (is (= \a (in/peek stream)))
    (is (= 0 (in/position stream)))
    (is (= \a (in/peek stream)))))

(deftest reset-position!-allows-to-move-the-cursor-arbitrarily
  (let [stream (in/make-stream "abc")]
    (is (= 0 (in/position stream)))
    (is (= \a (in/next! stream)))
    (is (= 1 (in/position stream)))
    (is (= \b (in/next! stream)))
    (in/reset-position! stream 1)
    (is (= 1 (in/position stream)))
    (is (= \b (in/next! stream)))
    (is (= 2 (in/position stream)))
    (is (= \c (in/next! stream)))
    (is (= 3 (in/position stream)))))

(deftest end?-only-returns-true-after-the-entire-stream-has-been-consumed
  (let [stream (in/make-stream "abc")]
    (is (not (in/end? stream)))
    (in/next! stream)
    (is (not (in/end? stream)))
    (in/next! stream)
    (in/next! stream)
    (is (in/end? stream))
    (dotimes [n 10]
             (in/next! stream))
    (is (in/end? stream))))

(deftest take!-returns-string-with-up-to-n-characters
  (let [stream (in/make-stream "abcdefghijklmnñopqrstuvwxyz")]
    (is (= "abc" (in/take! stream 3)))
    (is (= "def" (in/take! stream 3)))))

(deftest take!-on-empty-stream-returns-empty-string
  (let [stream (in/make-stream "")]
    (is (= "" (in/take! stream 3)))))

(deftest take!-stops-at-end-of-stream
  (let [stream (in/make-stream "abc")]
    (in/next! stream)
    (is (= "bc" (in/take! stream 30)))))

(deftest take!-after-end-returns-empty-string
  (let [stream (in/make-stream "abc")]
    (dotimes [n 3] (in/next! stream))
    (is (= "" (in/take! stream 30)))))
