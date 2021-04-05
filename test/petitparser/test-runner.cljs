(ns petitparser.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [petitparser.input-stream-test :as in]))

(enable-console-print!)
(run-tests 'petitparser.input-stream-test)
