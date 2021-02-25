(ns petitparser.parsers
  (:require [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :refer :all]))

(defprotocol Parser (parse-on [self stream]))

(deftype LiteralObjectParser [literal]
  Parser
  (parse-on [self stream]
            (if (= literal (in/peek stream))
              (success (in/next! stream))
              (failure (in/position stream)
                       (str "Literal '" literal "' expected")))))

(deftype LiteralSequenceParser [literal count]
  Parser
  (parse-on [self stream]
            (let [position (in/position stream)
                  result (in/take! stream count)]
              (if (= literal result)
                (success result)
                (do
                  (in/reset-position! stream position)
                  (failure position
                           (str "Literal '" literal "' expected")))))))

(deftype SequenceParser [parsers]
  Parser
  (parse-on [self stream]
            (let [position (in/position stream)
                  elements (map #(parse-on % stream)
                              parsers)]
              (if (every? success? elements)
                (success (mapv actual-result elements))
                (do
                  (in/reset-position! stream position)
                  (first (filter failure? elements)))))))

(deftype ChoiceParser [parsers]
  Parser
  (parse-on [self stream]
            (loop [parser (first parsers)
                   rest (next parsers)
                   fail (failure 0 "")]
              (if-let [result (if parser (parse-on parser stream))]
                (if (success? result)
                  result
                  (recur
                    (first rest)
                    (next rest)
                    result))
                fail))))

(deftype FlattenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (failure? result)
                result
                (success (subs (in/source stream)
                               start
                               (in/position stream)))))))

(deftype AndParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (in/reset-position! stream start)
              result)))

(deftype EndParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (or (failure? result)
                      (in/end? stream))
                result
                (let [fail (failure (in/position stream)
                                    "End of input expected")]
                  (in/reset-position! stream start)
                  fail)))))

(deftype RepeatingParser [parser ^long min ^long max]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (atom [])
                   failure (atom nil)]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (success? result)
                      (do
                        (swap! elements conj (actual-result result))
                        (recur (inc count)))
                      (do
                        (in/reset-position! stream start)
                        (reset! failure result))))))
              (if @failure
                @failure
                (do (loop [count 0]
                      (when (< count max)
                        (let [result (parse-on parser stream)]
                          (when (success? result)
                            (swap! elements conj (actual-result result))
                            (recur (inc count))))))
                  (success @elements))))))

(deftype GreedyRepeatingParser [parser min max limit]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  elements (atom [])
                  return (atom nil)]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (success? result)
                      (do
                        (swap! elements conj (actual-result result))
                        (recur (inc count)))
                      (do
                        (in/reset-position! stream start)
                        (reset! return result))))))
              (if @return
                @return
                (let [positions (atom [(in/position stream)])]
                  (loop [count (clojure.core/count @elements)]
                    (when (< count max)
                      (let [result (parse-on parser stream)]
                        (when (success? result)
                          (swap! elements conj (actual-result result))
                          (swap! positions conj (in/position stream))
                          (recur (inc count))))))
                  (loop [count (clojure.core/count @positions)]
                    (when (> count 0)
                      (in/reset-position! stream (last @positions))
                      (let [result (parse-on limit stream)]
                        (if (success? result)
                          (do
                            (in/reset-position! stream (last @positions))
                            (reset! return (success @elements)))
                          (if (empty? @elements)
                            (do
                              (in/reset-position! stream start)
                              (reset! return result))
                            (do
                              (swap! elements pop)
                              (swap! positions pop)
                              (recur (dec count))))))))
                  (if @return
                    @return
                    (failure start "Overflow")))))))

(deftype NotParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (in/reset-position! stream start)
              (if (success? result)
                (failure (in/position stream) "")
                (success nil)))))

(deftype OptionalParser [parser]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (success? result)
                result
                (success nil)))))

(deftype TokenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (failure? result)
                result
                (let [token (t/make-token (in/source stream)
                                          start
                                          (- (in/position stream) start)
                                          (actual-result result))]
                  (success token))))))

(deftype ActionParser [parser function]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (failure? result)
                result
                (success (function (actual-result result)))))))

(deftype PredicateObjectParser [function message]
  Parser
  (parse-on [self stream]
            (if (and (not (in/end? stream))
                     (function (in/peek stream)))
              (success (in/next! stream))
              (failure (in/position stream) message))))

(deftype PredicateSequenceParser [function message count]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (in/take! stream count)]
              (if (and (= count (clojure.core/count result))
                       (function result))
                (success result)
                (do
                  (in/reset-position! stream start)
                  (failure (in/position stream)
                           message))))))
