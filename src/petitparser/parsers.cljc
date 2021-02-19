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
            (let [results (map #(parse-on % stream)
                               parsers)]
              (if (every? failure? results)
                (last results)
                (first (filter success? results))))))

(deftype FlattenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (failure? result)
                result
                (success (subs (in/source stream)
                               start
                               (- (in/position stream)
                                  start)))))))

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

(deftype PredicateParser [function message]
  Parser
  (parse-on [self stream]
            (if (and (not (in/end? stream))
                     (function (in/peek stream)))
              (success (in/next! stream))
              (failure (in/position stream) message))))
