(ns petitparser.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [petitparser.core :as pp]
            [petitparser.input-stream-test :as in]
            [petitparser.core-test :as core]
            [petitparser.arithmetic-parser-test :as arith]))

(enable-console-print!)
(run-tests 'petitparser.input-stream-test
           'petitparser.core-test
           'petitparser.arithmetic-parser-test)

(defn parse [str]
  (pp/parse arith/pp str))
