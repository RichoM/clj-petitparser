(ns petitparser.parsers
  (:require [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :as r]))

(defprotocol Parser (parse-on [self stream]))

(defrecord LiteralObjectParser [literal]
  Parser
  (parse-on [self stream]
            (if (= literal (in/peek stream))
              (r/success (in/next! stream))
              (r/failure (in/position stream)
                         (str "Literal '" literal "' expected")))))

(defrecord LiteralSequenceParser [literal count]
  Parser
  (parse-on [self stream]
            (let [position (in/position stream)
                  result (in/take! stream count)]
              (if (= literal result)
                (r/success result)
                (do
                  (in/reset-position! stream position)
                  (r/failure position
                             (str "Literal '" literal "' expected")))))))

(defrecord SequenceParser [parsers]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (volatile! [])
                   return (volatile! nil)]
              (loop [parser (first parsers)
                     rest (next parsers)]
                (when parser
                  (let [result (parse-on parser stream)]
                    (if (r/success? result)
                      (do
                        (vswap! elements conj (r/actual-result result))
                        (recur
                          (first rest)
                          (next rest)))
                      (do
                        (in/reset-position! stream start)
                        (vreset! return result))))))
              (if @return
                @return
                (r/success @elements)))))

(defrecord ChoiceParser [parsers]
  Parser
  (parse-on [self stream]
            (loop [parser (first parsers)
                   rest (next parsers)
                   fail (r/failure 0 "")]
              (if-let [result (if parser (parse-on parser stream))]
                (if (r/success? result)
                  result
                  (recur
                    (first rest)
                    (next rest)
                    result))
                fail))))

(defrecord FlattenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (parse-on parser stream)]
              (if (r/failure? result)
                result
                (r/success (subs (in/source stream)
                                 start
                                 (in/position stream)))))))

(defrecord AndParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (in/reset-position! stream start)
              result)))

(defrecord EndParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (parse-on parser stream)]
              (if (or (r/failure? result)
                      (in/end? stream))
                result
                (let [fail (r/failure (in/position stream)
                                      "End of input expected")]
                  (in/reset-position! stream start)
                  fail)))))

(defrecord RepeatingParser [parser ^long min ^long max]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (volatile! [])
                   failure (volatile! nil)]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (r/success? result)
                      (do
                        (vswap! elements conj (r/actual-result result))
                        (recur (inc count)))
                      (do
                        (in/reset-position! stream start)
                        (vreset! failure result))))))
              (if @failure
                @failure
                (do (loop [count 0]
                      (when (< count max)
                        (let [result (parse-on parser stream)]
                          (when (r/success? result)
                            (vswap! elements conj (r/actual-result result))
                            (recur (inc count))))))
                  (r/success @elements))))))

(defrecord GreedyRepeatingParser [parser ^long min ^long max limit]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (volatile! [])
                   return (volatile! nil)]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (r/success? result)
                      (do
                        (vswap! elements conj (r/actual-result result))
                        (recur (inc count)))
                      (do
                        (in/reset-position! stream start)
                        (vreset! return result))))))
              (if @return
                @return
                (let [positions (volatile! [(in/position stream)])]
                  (loop [count (clojure.core/count @elements)]
                    (when (< count max)
                      (let [result (parse-on parser stream)]
                        (when (r/success? result)
                          (vswap! elements conj (r/actual-result result))
                          (vswap! positions conj (in/position stream))
                          (recur (inc count))))))
                  (loop [count (clojure.core/count @positions)]
                    (when (> count 0)
                      (in/reset-position! stream (last @positions))
                      (let [result (parse-on limit stream)]
                        (if (r/success? result)
                          (do
                            (in/reset-position! stream (last @positions))
                            (vreset! return (r/success @elements)))
                          (if (empty? @elements)
                            (do
                              (in/reset-position! stream start)
                              (vreset! return result))
                            (do
                              (vswap! elements pop)
                              (vswap! positions pop)
                              (recur (dec count))))))))
                  (if @return
                    @return
                    (r/failure start "Overflow")))))))

(defrecord LazyRepeatingParser [parser ^long min ^long max limit]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (volatile! [])
                   return (volatile! nil)
                   matches-limit? #(let [start (in/position stream)
                                          result (parse-on limit stream)]
                                     (in/reset-position! stream start)
                                     (r/success? result))]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (r/success? result)
                      (do
                        (vswap! elements conj (r/actual-result result))
                        (recur (inc count)))
                      (do
                        (in/reset-position! stream start)
                        (vreset! return result))))))
              (if @return
                @return
                (do
                  (loop [count (clojure.core/count @elements)]
                    (when-not (matches-limit?)
                      (if (>= count max)
                        (do
                          (in/reset-position! stream start)
                          (vreset! return (r/failure start "Overflow")))
                        (let [result (parse-on parser stream)]
                          (if (r/failure? result)
                            (do
                              (in/reset-position! stream start)
                              (vreset! return result))
                            (do
                              (vswap! elements conj (r/actual-result result))
                              (recur (inc count))))))))
                  (if @return
                    @return
                    (r/success @elements)))))))

(defrecord NotParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (parse-on parser stream)]
              (in/reset-position! stream start)
              (if (r/success? result)
                (r/failure (in/position stream) "")
                (r/success nil)))))

(defrecord OptionalParser [parser]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (r/success? result)
                result
                (r/success nil)))))

(defrecord TokenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (parse-on parser stream)]
              (if (r/failure? result)
                result
                (let [token (t/make-token (in/source stream)
                                          start
                                          (- (in/position stream) start)
                                          (r/actual-result result))]
                  (r/success token))))))

(defrecord ActionParser [parser function]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (r/failure? result)
                result
                (r/success (function (r/actual-result result)))))))

(defrecord TrimmingParser [parser trimmer]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   trim #(loop []
                           (if (r/success? (parse-on trimmer stream))
                             (recur)))]
              (trim)
              (let [result (parse-on parser stream)]
                (if (r/failure? result)
                  (do
                    (in/reset-position! stream start)
                    result)
                  (do
                    (trim)
                    result))))))

(defrecord PredicateObjectParser [function message]
  Parser
  (parse-on [self stream]
            (if (and (not (in/end? stream))
                     (function (in/peek stream)))
              (r/success (in/next! stream))
              (r/failure (in/position stream) message))))

(defrecord PredicateSequenceParser [function message count]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (in/take! stream count)]
              (if (and (= count (clojure.core/count result))
                       (function result))
                (r/success result)
                (do
                  (in/reset-position! stream start)
                  (r/failure (in/position stream)
                             message))))))

(defrecord PlaceholderParser [key]
  Parser
  (parse-on [self stream]
            (r/failure (in/position stream)
                       (format "Parser not found for keyword %s" key))))

(defrecord DelegateParser [parser]
  Parser
  (parse-on [self stream]
            (parse-on @parser stream)))

(defrecord CompositeParser [parsers start]
  Parser
  (parse-on [self stream]
            (parse-on start stream)))
