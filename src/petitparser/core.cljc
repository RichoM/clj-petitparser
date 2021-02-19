(ns petitparser.core
  (:refer-clojure :exclude [or flatten and min max not])
  (:require [clojure.core :as clj]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]))

(defprotocol Parser (parse-on [self stream]))

(defprotocol ParseResult
  (success? [self])
  (failure? [self])
  (actual-result [self]))

(deftype ParseSuccess [result]
  ParseResult
  (success? [_] true)
  (failure? [_] false)
  (actual-result [_] result))

(deftype ParseFailure [position message]
  ParseResult
  (success? [_] false)
  (failure? [_] true)
  (actual-result [_]
                 (throw (ex-info (format "%s at %d" message position)
                                 {:position position}))))

(defn- success [result]
  (ParseSuccess. result))

(defn- failure
  ([position message & args]
   (failure position (apply format message args)))
  ([position message]
   (ParseFailure. position message)))

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
              (if (clj/or (failure? result)
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
            (if (clj/and (clj/not (in/end? stream))
                         (function (in/peek stream)))
              (success (in/next! stream))
              (failure (in/position stream) message))))

(defmulti as-parser class)

(defmethod as-parser java.lang.Character [char]
  (LiteralObjectParser. char))

(defmethod as-parser java.lang.String [str]
  (LiteralSequenceParser. str (count str)))

(defmethod as-parser java.util.List [parsers]
  (SequenceParser. (mapv as-parser parsers)))

(defmethod as-parser petitparser.core.Parser [parser] parser)

(defn or [& parsers]
  (ChoiceParser. (mapv as-parser parsers)))

(defn and [parser]
  (AndParser. (as-parser parser)))

(defn flatten [parser]
  (FlattenParser. (as-parser parser)))

(defn end [parser]
  (EndParser. (as-parser parser)))

(defn star [parser]
  (RepeatingParser. (as-parser parser) 0 Integer/MAX_VALUE))

(defn plus [parser]
  (RepeatingParser. (as-parser parser) 1 Integer/MAX_VALUE))

(defn times [parser n]
  (RepeatingParser. (as-parser parser) n n))

(defn min [parser n]
  (RepeatingParser. (as-parser parser) n Integer/MAX_VALUE))

(defn max [parser n]
  (RepeatingParser. (as-parser parser) 0 n))

(defn not [parser]
  (NotParser. (as-parser parser)))

(defn optional [parser]
  (OptionalParser. (as-parser parser)))

(defn token [parser]
  (TokenParser. (as-parser parser)))

(defn transform [parser function]
  (ActionParser. (as-parser parser) function))

(defn predicate [function message]
  (PredicateParser. function message))

(defn- digit? [^Character chr] (Character/isDigit chr))
(defn- letter? [^Character chr] (Character/isLetter chr))
(defn- letter-or-digit? [^Character chr] (Character/isLetterOrDigit chr))
(defn- whitespace? [^Character chr] (Character/isWhitespace chr))

(def any (predicate (constantly true) "Input expected"))
(def digit (predicate digit? "Digit expected"))
(def letter (predicate letter? "Letter expected"))
(def word (predicate letter-or-digit? "Letter or digit expected"))
(def space (predicate whitespace? "White space expected"))

(defn parse [parser src]
  (actual-result (parse-on parser (in/make-stream src))))

(defn matches? [parser src]
  (success? (parse-on parser (in/make-stream src))))

(comment
 ,)
