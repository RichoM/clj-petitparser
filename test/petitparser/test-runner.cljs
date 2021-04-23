(ns petitparser.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [petitparser.core :as pp]
            [petitparser.input-stream-test]
            [petitparser.core-test]
            [petitparser.arithmetic-parser-test]
            [middleware.parser-test]
            [middleware.parser.parser :as uzi]))

(enable-console-print!)
(run-tests 'petitparser.input-stream-test
           'petitparser.core-test
           'petitparser.arithmetic-parser-test
           'middleware.parser-test)

(defn parse [src]
  (clj->js (uzi/parse src)))
