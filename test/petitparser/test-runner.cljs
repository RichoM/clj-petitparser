(ns petitparser.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [petitparser.input-stream-test :as in]
            [petitparser.core-test :as core]))

(enable-console-print!)
(run-tests 'petitparser.input-stream-test
           'petitparser.core-test)
