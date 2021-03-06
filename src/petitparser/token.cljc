(ns petitparser.token
  (:refer-clojure :exclude [count]))

(defn make-token [source start count value]
  {:source source
   :start start
   :count count
   :parsed-value value})

(defn source [token] (:source token))
(defn start [token] (:start token))
(defn count [token] (:count token))
(defn parsed-value [token] (:parsed-value token))

(defn stop [{:keys [^long start ^long count]}]
  (+ start count))

(defn input-value [{:keys [source ^long start ^long count]}]
  (subs source start (+ start count)))
