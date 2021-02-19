(ns petitparser.input-stream-test
  (:refer-clojure :exclude [peek])
  (:require [clojure.test :refer :all]
            [petitparser.input-stream :refer :all]))

(deftest next!-increments-position
  (let [stream (make-stream "abc")]
    (is (= 0 (position stream)))
    (is (= \a (next! stream)))
    (is (= 1 (position stream)))
    (is (= \b (next! stream)))
    (is (= 2 (position stream)))
    (is (= \c (next! stream)))
    (is (= 3 (position stream)))))

(deftest peek-does-not-increment-position
  (let [stream (make-stream "abc")]
    (is (= 0 (position stream)))
    (is (= \a (peek stream)))
    (is (= 0 (position stream)))
    (is (= \a (peek stream)))))

(deftest reset-position!-allows-to-move-the-cursor-arbitrarily
  (let [stream (make-stream "abc")]
    (is (= 0 (position stream)))
    (is (= \a (next! stream)))
    (is (= 1 (position stream)))
    (is (= \b (next! stream)))
    (reset-position! stream 1)
    (is (= 1 (position stream)))
    (is (= \b (next! stream)))
    (is (= 2 (position stream)))
    (is (= \c (next! stream)))
    (is (= 3 (position stream)))))

(deftest end?-only-returns-true-after-the-entire-stream-has-been-consumed
  (let [stream (make-stream "abc")]
    (is (not (end? stream)))
    (next! stream)
    (is (not (end? stream)))
    (next! stream)
    (next! stream)
    (is (end? stream))
    (dotimes [n 10]
             (next! stream))
    (is (end? stream))))

(deftest take!-returns-string-with-up-to-n-characters
  (let [stream (make-stream "abcdefghijklmnñopqrstuvwxyz")]
    (is (= "abc" (take! stream 3)))
    (is (= "def" (take! stream 3)))))

(deftest take!-on-empty-stream-returns-empty-string
  (let [stream (make-stream "")]
    (is (= "" (take! stream 3)))))

(deftest take!-stops-at-end-of-stream
  (let [stream (make-stream "abc")]
    (next! stream)
    (is (= "bc" (take! stream 30)))))

(deftest take!-after-end-returns-empty-string
  (let [stream (make-stream "abc")]
    (dotimes [n 3] (next! stream))
    (is (= "" (take! stream 30)))))
