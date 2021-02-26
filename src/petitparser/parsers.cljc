(ns petitparser.parsers
  (:require [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :refer :all]))

(defprotocol Parser (parse-on [self stream]))

(defrecord LiteralObjectParser [literal]
  Parser
  (parse-on [self stream]
            (if (= literal (in/peek stream))
              (success (in/next! stream))
              (failure (in/position stream)
                       (str "Literal '" literal "' expected")))))

(defrecord LiteralSequenceParser [literal count]
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

(defrecord SequenceParser [parsers]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (atom [])
                   return (atom nil)]
              (loop [parser (first parsers)
                     rest (next parsers)]
                (when parser
                  (let [result (parse-on parser stream)]
                    (if (success? result)
                      (do
                        (swap! elements conj (actual-result result))
                        (recur
                          (first rest)
                          (next rest)))
                      (do
                        (in/reset-position! stream start)
                        (reset! return result))))))
              (if @return
                @return
                (success @elements)))))

(defrecord ChoiceParser [parsers]
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

(defrecord FlattenParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (failure? result)
                result
                (success (subs (in/source stream)
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
              (if (or (failure? result)
                      (in/end? stream))
                result
                (let [fail (failure (in/position stream)
                                    "End of input expected")]
                  (in/reset-position! stream start)
                  fail)))))

(defrecord RepeatingParser [parser ^long min ^long max]
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

(defrecord GreedyRepeatingParser [parser ^long min ^long max limit]
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

(defrecord LazyRepeatingParser [parser ^long min ^long max limit]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (atom [])
                   return (atom nil)
                   matches-limit? #(let [start (in/position stream)
                                          result (parse-on limit stream)]
                                     (in/reset-position! stream start)
                                     (success? result))]
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
                (do
                  (loop [count (clojure.core/count @elements)]
                    (when-not (matches-limit?)
                      (if (>= count max)
                        (do
                          (in/reset-position! stream start)
                          (reset! return (failure start "Overflow")))
                        (let [result (parse-on parser stream)]
                          (if (failure? result)
                            (do
                              (in/reset-position! stream start)
                              (reset! return result))
                            (do
                              (swap! elements conj (actual-result result))
                              (recur (inc count))))))))
                  (if @return
                    @return
                    (success @elements)))))))

(defrecord NotParser [parser]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (in/reset-position! stream start)
              (if (success? result)
                (failure (in/position stream) "")
                (success nil)))))

(defrecord OptionalParser [parser]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (success? result)
                result
                (success nil)))))

(defrecord TokenParser [parser]
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

(defrecord ActionParser [parser function]
  Parser
  (parse-on [self stream]
            (let [result (parse-on parser stream)]
              (if (failure? result)
                result
                (success (function (actual-result result)))))))

(defrecord TrimmingParser [parser trimmer]
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   trim #(loop []
                           (if (success? (parse-on trimmer stream))
                             (recur)))]
              (trim)
              (let [result (parse-on parser stream)]
                (if (failure? result)
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
              (success (in/next! stream))
              (failure (in/position stream) message))))

(defrecord PredicateSequenceParser [function message count]
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

(defrecord PlaceholderParser [key]
  Parser
  (parse-on [self stream]
            (failure (in/position stream)
                     (format "Parser not found for keyword %s" key))))

(defrecord DelegateParser [parser]
  Parser
  (parse-on [self stream]
            (parse-on @parser stream)))

(defrecord CompositeParser [parsers]
  Parser
  (parse-on [self stream]
            (parse-on (:start parsers) stream)))

(defmethod print-method DelegateParser [v ^java.io.Writer w]
  (.write w "DelegateParser"))
