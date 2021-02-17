(ns clj-petitparser.core
  (:require [clj-petitparser.input-stream :as in]))

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
                (let [fail (failure (in/position stream)
                                    (str "Literal '" literal "' expected"))]
                  (in/reset-position! stream position)
                  fail)))))

(deftype SequenceParser [parsers]
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

(defprotocol ParserBuilder (as-parser [self]))

(extend-type java.lang.Character
  ParserBuilder
  (as-parser [char] (LiteralObjectParser. char)))

(extend-type java.lang.String
  ParserBuilder
  (as-parser [str] (LiteralSequenceParser. str (count str))))

(extend-type java.util.List
  ParserBuilder
  (as-parser [parsers] (SequenceParser. (mapv as-parser parsers))))


(defn parse [parser src]
  (actual-result (parse-on parser (in/make-stream src))))

(comment
 (require '[clj-petitparser.input-stream :as in])
 (class \a)
 (class "Richo")
 (ancestors (type (seq [1 2 3])))
 clojure.lang.ISeq


 (def stream (in/make-stream "Richo capo"))
 (def parser (as-parser \a))

 (parse parser "a")



 (apply format "%s at %d" ["Richo expected" 1])

 (parse (as-parser [\a \b \c]) "abd")




 ,)
