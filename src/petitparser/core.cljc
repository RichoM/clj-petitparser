(ns petitparser.core
  (:refer-clojure :exclude [or flatten and min max not seq])
  (:require [clojure.core :as clj]
            [clojure.string :as str]
            [clojure.walk :as w]
            [petitparser.parsers :as parsers]
            [petitparser.results :as r]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]))

(def MAX_VALUE
  #?(:clj Integer/MAX_VALUE
     :cljs (.-MAX_SAFE_INTEGER js/Number)))

#?(:clj (defmulti as-parser type))

#?(:clj (defmethod as-parser java.lang.Character [char]
          (petitparser.parsers.LiteralObjectParser. char)))

#?(:clj (defmethod as-parser java.lang.String [str]
          (petitparser.parsers.LiteralSequenceParser. str (count str))))

#?(:clj (defmethod as-parser java.util.List [parsers]
          (petitparser.parsers.SequenceParser. (mapv as-parser parsers))))

#?(:clj (defmethod as-parser clojure.lang.Keyword [keyword]
          (petitparser.parsers.PlaceholderParser. keyword)))

#?(:clj (defmethod as-parser petitparser.parsers.Parser [parser] parser))

#?(:cljs (defn as-parser [obj]
           (cond
             (string? obj) (petitparser.parsers.LiteralSequenceParser. obj (count obj))
             (keyword? obj) (petitparser.parsers.PlaceholderParser. obj)
             (vector? obj) (petitparser.parsers.SequenceParser. (mapv as-parser obj))
             (satisfies? petitparser.parsers/Parser obj) obj)))

(defn seq [& parsers]
  (as-parser (vec parsers)))

(defn or [& parsers]
  (petitparser.parsers.ChoiceParser. (mapv as-parser parsers)))

(defn and [parser]
  (petitparser.parsers.AndParser. (as-parser parser)))

(defn flatten [parser]
  (petitparser.parsers.FlattenParser. (as-parser parser)))

(defn end [parser]
  (petitparser.parsers.EndParser. (as-parser parser)))

(defn star [parser]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 0 MAX_VALUE))

(defn plus [parser]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 1 MAX_VALUE))

(defn times [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) n n))

(defn min [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) n MAX_VALUE))

(defn max [parser n]
  (petitparser.parsers.RepeatingParser. (as-parser parser) 0 n))

(defn plus-greedy [parser limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) 1 MAX_VALUE (as-parser limit)))

(defn star-greedy [parser limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) 0 MAX_VALUE (as-parser limit)))

(defn plus-lazy [parser limit]
  (petitparser.parsers.LazyRepeatingParser. (as-parser parser) 1 MAX_VALUE (as-parser limit)))

(defn star-lazy [parser limit]
  (petitparser.parsers.LazyRepeatingParser. (as-parser parser) 0 MAX_VALUE (as-parser limit)))

(defn min-greedy [parser min limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) min MAX_VALUE (as-parser limit)))

(defn max-greedy [parser max limit]
  (petitparser.parsers.GreedyRepeatingParser. (as-parser parser) 0 max (as-parser limit)))

(defn min-lazy [parser min limit]
  (petitparser.parsers.LazyRepeatingParser. (as-parser parser) min MAX_VALUE (as-parser limit)))

(defn max-lazy [parser max limit]
  (petitparser.parsers.LazyRepeatingParser. (as-parser parser) 0 max (as-parser limit)))

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

#?(:clj (defn digit? [^Character chr] (Character/isDigit chr))
   :cljs (defn digit? [chr] (re-matches #"\d" chr)))

#?(:clj (defn letter? [^Character chr] (Character/isLetter chr))
   :cljs (defn letter? [chr] (re-matches #"\p{L}" chr)))

#?(:clj (defn letter-or-digit? [^Character chr] (Character/isLetterOrDigit chr))
   :cljs (defn letter-or-digit? [chr] (re-matches #"\w" chr)))

#?(:clj (defn whitespace? [^Character chr] (Character/isWhitespace chr))
   :cljs (defn whitespace? [chr] (re-matches #"\s" chr)))

(comment
(defn- digit? [chr] (re-matches #"\p{N}" (str chr)))
(defn- letter? [chr] (re-matches #"\p{L}" (str chr)))
(defn- letter-or-digit? [chr] (re-matches #"\p{Nl}" (str chr)))
(defn- whitespace? [chr] (re-matches #"\s" (str chr)))

(time (dotimes [_ 1000000] (digit? \1)))
(Character/isDigit \u0660)
(letter-or-digit? \u0660)
\u001C
(Double/parseDouble (str \1))



(set! *print-length* 100)

)

(def any (predicate (constantly true) "Input expected"))
(def digit (predicate digit? "Digit expected"))
(def letter (predicate letter? "Letter expected"))
(def word (predicate letter-or-digit? "Letter or digit expected"))
(def space (predicate whitespace? "White space expected"))

(defn trim
  ([parser] (trim parser space))
  ([parser trimmer]
   (petitparser.parsers.TrimmingParser. (as-parser parser)
                                        (as-parser trimmer))))

(defn predicate-sequence [function message count]
  (petitparser.parsers.PredicateSequenceParser. function message count))

#?(:clj (defmulti case-insensitive type))

#?(:clj (defmethod case-insensitive
          petitparser.parsers.LiteralObjectParser
          [^petitparser.parsers.LiteralObjectParser parser]
          (let [literal (.literal parser)]
            (if (= (str/upper-case literal)
                   (str/lower-case literal))
              parser
              (let [lower (str/lower-case literal)]
                (predicate (fn [char] (= lower (str/lower-case char)))
                           (str "Literal '" lower "' expected")))))))

#?(:clj (defmethod case-insensitive
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
                 (count lower)))))))

#?(:clj (defmethod case-insensitive
          petitparser.parsers.Parser
          [parser]
          (w/prewalk (fn [each]
                       (if (clj/or (instance? petitparser.parsers.LiteralSequenceParser each)
                                   (instance? petitparser.parsers.LiteralObjectParser each))
                         (case-insensitive each)
                         each))
                     parser)))

#?(:clj (defmethod case-insensitive java.lang.Character [char]
          (case-insensitive (as-parser char))))

#?(:clj (defmethod case-insensitive java.lang.String [str]
          (case-insensitive (as-parser str))))

#?(:clj (defmethod case-insensitive :default [obj]
        (case-insensitive (as-parser obj))))

#?(:cljs (defn case-insensitive [parser]
           (cond
             (instance? petitparser.parsers.LiteralObjectParser parser)
             (let [literal (.-literal parser)]
               (if (= (str/upper-case literal)
                      (str/lower-case literal))
                 parser
                 (let [lower (str/lower-case literal)]
                   (predicate (fn [char] (= lower (str/lower-case char)))
                              (str "Literal '" lower "' expected")))))

             (instance? petitparser.parsers.LiteralSequenceParser parser)
             (let [literal (.-literal parser)]
               (if (= (str/upper-case literal)
                      (str/lower-case literal))
                 parser
                 (let [lower (str/lower-case literal)]
                   (predicate-sequence
                    (fn [str] (= lower (str/lower-case str)))
                    (str "Literal '" lower "' expected")
                    (count lower)))))

             (satisfies? petitparser.parsers/Parser parser)
             (w/prewalk (fn [each]
                          (if (clj/or (instance? petitparser.parsers.LiteralSequenceParser each)
                                      (instance? petitparser.parsers.LiteralObjectParser each))
                            (case-insensitive each)
                            each))
                        parser)

             :else (case-insensitive (as-parser parser)))))


(defn negate [parser]
  (transform [(not parser) any]
             second))

(defn separated-by [parser separator]
  (transform [parser (star [separator parser])]
             (fn [[f s]]
               (loop [[p0 p1] (first s)
                      rest (next s)
                      result (transient [f])]
                 (if p0
                   (recur
                     (first rest)
                     (next rest)
                     (-> result
                         (conj! p0)
                         (conj! p1)))
                   (persistent! result))))))

(defn- delegate []
  (petitparser.parsers.DelegateParser. (atom nil)))

(defn- resolve! [^petitparser.parsers.DelegateParser delegate parser]
  (reset! (:parser delegate) parser))

(defn compose
  ([grammar] (compose grammar {}))
  ([grammar transformations] (compose grammar transformations :start))
  ([grammar transformations start]
   (let [; Change all keys to delegate parsers
         parser
         (into {}
               (map (fn [[key _]] [key (delegate)])
                    grammar))

         ; Replace placeholders with the actual parsers
         actual-grammar
         (into {}
               (map (fn [[key val]]
                      [key (w/prewalk (fn [each]
                                        (if (instance? petitparser.parsers.PlaceholderParser
                                                       each)
                                          (let [^petitparser.parsers.PlaceholderParser placeholder each
                                                key (:key placeholder)]
                                            (clj/or (get parser key)
                                                    (throw (ex-info (str "Grammar not found for keyword " key)
                                                                    {:grammar grammar}))))
                                          each))
                                      (as-parser val))])
                    grammar))]

     ; Resolve all delegate parsers (apply transformations!)
     (doseq [key (keys actual-grammar)]
       (resolve! (get parser key)
                 (if-let [tran (get transformations key)]
                   (transform (get actual-grammar key) tran)
                   (get actual-grammar key))))

     ; Return the composite parser
     (petitparser.parsers.CompositeParser. parser (start parser)))))

(def parse-on parsers/parse-on)

(defn parse [parser src]
  (r/actual-result (parse-on parser (in/make-stream src))))

(defn matches? [parser src]
  (r/success? (parse-on parser (in/make-stream src))))
