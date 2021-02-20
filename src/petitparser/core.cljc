(ns petitparser.core
  (:refer-clojure :exclude [or flatten and min max not])
  (:require [clojure.core :as clj]
            [clojure.string :as str]
            [petitparser.parsers :as parsers]
            [petitparser.results :refer :all]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]))

(defmulti as-parser class)

(defmethod as-parser java.lang.Character [char]
  (petitparser.parsers.LiteralObjectParser. char))

(defmethod as-parser java.lang.String [str]
  (petitparser.parsers.LiteralSequenceParser. str (count str)))

(defmethod as-parser java.util.List [parsers]
  (petitparser.parsers.SequenceParser. (mapv as-parser parsers)))

(defmethod as-parser petitparser.parsers.Parser [parser] parser)

(defn or [& parsers]
  (petitparser.parsers.ChoiceParser. (mapv as-parser parsers)))

(defn and [parser]
  (petitparser.parsers.AndParser. (as-parser parser)))

(defn flatten [parser]
  (petitparser.parsers.FlattenParser. (as-parser parser)))

(defn end [parser]
  (petitparser.parsers.EndParser. (as-parser parser)))

(defn star [parser]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 0 Integer/MAX_VALUE))

(defn plus [parser]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 1 Integer/MAX_VALUE))

(defn times [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) n n))

(defn min [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) n Integer/MAX_VALUE))

(defn max [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 0 n))

(defn plus-greedy [parser limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) 1 Integer/MAX_VALUE (as-parser limit)))

(defn star-greedy [parser limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) 0 Integer/MAX_VALUE (as-parser limit)))

(defn not [parser]
  (petitparser.parsers.NotParser. (as-parser parser)))

(defn optional [parser]
  (petitparser.parsers.OptionalParser. (as-parser parser)))

(defn token [parser]
  (petitparser.parsers.TokenParser. (as-parser parser)))

(defn transform [parser function]
  (petitparser.parsers.ActionParser. (as-parser parser) function))

(defn predicate [function message]
  (petitparser.parsers.PredicateObjectParser. function message))

(defn- digit? [^Character chr] (Character/isDigit chr))
(defn- letter? [^Character chr] (Character/isLetter chr))
(defn- letter-or-digit? [^Character chr] (Character/isLetterOrDigit chr))
(defn- whitespace? [^Character chr] (Character/isWhitespace chr))

(def any (predicate (constantly true) "Input expected"))
(def digit (predicate digit? "Digit expected"))
(def letter (predicate letter? "Letter expected"))
(def word (predicate letter-or-digit? "Letter or digit expected"))
(def space (predicate whitespace? "White space expected"))

(defn predicate-sequence [function message count]
  (petitparser.parsers.PredicateSequenceParser. function message count))

(defmulti case-insensitive class)

(defmethod case-insensitive
  petitparser.parsers.LiteralObjectParser
  [^petitparser.parsers.LiteralObjectParser parser]
  (let [literal (.literal parser)]
    (if (= (str/upper-case literal)
           (str/lower-case literal))
      parser
      (let [lower (str/lower-case literal)]
        (predicate (fn [char] (= lower (str/lower-case char)))
                   (str "Literal '" lower "' expected"))))))

(defmethod case-insensitive
  petitparser.parsers.LiteralSequenceParser
  [^petitparser.parsers.LiteralSequenceParser parser]
  (let [literal (.literal parser)]
    (if (= (str/upper-case literal)
           (str/lower-case literal))
      parser
      (let [lower (str/lower-case literal)]
        (predicate-sequence
         (fn [str] (= lower (str/lower-case str)))
         (str "Literal '" lower "' expected")
         (count lower))))))

(defmethod case-insensitive java.lang.Character [char]
  (case-insensitive (as-parser char)))

(defmethod case-insensitive java.lang.String [str]
  (case-insensitive (as-parser str)))

(def parse-on parsers/parse-on)

(defn parse [parser src]
  (actual-result (parse-on parser (in/make-stream src))))

(defn matches? [parser src]
  (success? (parse-on parser (in/make-stream src))))

(comment
 (def pp (end [(flatten (plus-greedy "a" "ab"))
               "abc"
               (flatten (star digit))]))
 (parse pp "aaaaabc42")
 ,)
