(ns clj-petitparser.core
  (:refer-clojure :exclude [or])
  (:require [clj-petitparser.input-stream :as in]))

(defprotocol ParserBuilder (as-parser [self]))

(defprotocol Parser (parse-on [self stream]))

(defprotocol ParseResult
  (is-success? [self])
  (is-failure? [self])
  (actual-result [self]))

(deftype ParseSuccess [result]
  ParseResult
  (is-success? [_] true)
  (is-failure? [_] false)
  (actual-result [_] result))

(deftype ParseFailure [position message]
  ParseResult
  (is-success? [_] false)
  (is-failure? [_] true)
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
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (if (= literal (in/peek stream))
              (success (in/next! stream))
              (failure (in/position stream)
                       (str "Literal '" literal "' expected")))))

(deftype LiteralSequenceParser [literal count]
  ParserBuilder
  (as-parser [self] self)
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
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [position (in/position stream)
                  elements (map #(parse-on % stream)
                              parsers)]
              (if (every? is-success? elements)
                (success (mapv actual-result elements))
                (do
                  (in/reset-position! stream position)
                  (first (filter is-failure? elements)))))))

(deftype ChoiceParser [parsers]
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [results (map #(parse-on % stream)
                               parsers)]
              (if (every? is-failure? results)
                (last results)
                (first (filter is-success? results))))))

(deftype FlattenParser [parser]
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   result (parse-on parser stream)]
              (if (is-failure? result)
                result
                (success (subs (in/source stream)
                               start
                               (- (in/position stream)
                                  start)))))))

(extend-type java.lang.Character
  ParserBuilder
  (as-parser [char] (LiteralObjectParser. char)))

(extend-type java.lang.String
  ParserBuilder
  (as-parser [str] (LiteralSequenceParser. str (count str))))

(extend-type java.util.List
  ParserBuilder
  (as-parser [parsers] (SequenceParser. (mapv as-parser parsers))))

(defn or [& parsers]
  (ChoiceParser. (mapv as-parser parsers)))

(defn flatten [parser]
  (FlattenParser. (as-parser parser)))

(defn parse [parser src]
  (actual-result (parse-on parser (in/make-stream src))))

(comment
 (require '[clj-petitparser.input-stream :as in])
 (class \a)
 (class "Richo")
 (ancestors (type (seq [1 2 3])))
 clojure.lang.ISeq


 (def stream (in/make-stream "Richo capo"))
 (def parser (as-parser [(or "perro"
                             "gato"
                             "león")
                         " "
                         (or "hambriento"
                             "cansado"
                             "feliz")]))

 (parse parser "gato feliz")

 (parse parser "a")



 (apply format "%s at %d" ["Richo expected" 1])

 (parse (as-parser ["richo" "capo"]) "richocapo")

 (parse (as-parser [\a \b \c]) "abd")

 (ancestors (type (as-parser "richo")))


 ,)
