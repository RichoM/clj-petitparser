(ns clj-petitparser.core
  (:refer-clojure :exclude [or flatten])
  (:require [clojure.core :as clj]
            [clj-petitparser.input-stream :as in]))

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

(deftype AndParser [parser]
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (in/reset-position! stream start)
              result)))

(deftype EndParser [parser]
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                  result (parse-on parser stream)]
              (if (clj/or (is-failure? result)
                          (in/end? stream))
                result
                (let [fail (failure (in/position stream)
                                    "End of input expected")]
                  (in/reset-position! stream start)
                  fail)))))

(deftype RepeatingParser [parser ^long min ^long max]
  ParserBuilder
  (as-parser [self] self)
  Parser
  (parse-on [self stream]
            (let [start (in/position stream)
                   elements (atom [])
                   failure (atom nil)]
              (loop [count 0]
                (when (< count min)
                  (let [result (parse-on parser stream)]
                    (if (is-success? result)
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
                          (when (is-success? result)
                            (swap! elements conj (actual-result result))
                            (recur (inc count))))))
                  (success @elements))))))

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

 (class (repeatedly 11 (fn [] (in/next! stream))))

 (parse parser "gato feliz")

 (parse (star "foo") "foofoofoo")


 Integer/MAX_VALUE

 (apply format "%s at %d" ["Richo expected" 1])

 (parse (as-parser ["richo" "capo"]) "richocapo")

 (parse (as-parser [\a \b \c]) "abd")

 (ancestors (type (as-parser "richo")))

 (parse-on (RepeatingParser. (as-parser "foo") 4 Integer/MAX_VALUE)
           (in/make-stream "foofoobarfoo"))

 (do
   (def parser (as-parser "a"))
   (def stream (in/make-stream "aaaaab"))
   (def results (repeatedly #(parse-on parser stream))))

 (mapv actual-result (take-while is-success? results))
 (.position (first (drop-while is-success? results)))
 (in/end? stream)
 (in/position stream)
 (in/peek stream)
 (realized? results)
 (vec results)





 (def elements (atom []))
 (time (dotimes [i 100000]
                (swap! elements conj i)))
 @elements


 (def elements (atom (transient [])))
 (time (dotimes [i 100000]
                (swap! elements conj! i)))
 (persistent! @elements)










 ,)
