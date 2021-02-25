(ns petitparser.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [petitparser.core :as pp]
            [petitparser.input-stream :as in]
            [petitparser.token :as t]
            [petitparser.results :as r]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(deftest literal-object-parser
  (let [pp (pp/as-parser \a)]
    (is (= \a (pp/parse pp "a")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w' expected"
                          (pp/parse pp "b")))))

(deftest literal-sequence-parser
  (let [pp (pp/as-parser "abc")]
    (is (= "abc" (pp/parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "abd")))))

(deftest sequence-parser
  (let [pp (pp/as-parser [\a \b \c])]
    (is (= [\a \b \c] (pp/parse pp "abc")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "abd")))))

(deftest choice-parser
  (let [pp (pp/or "perro"
                  "gato")]
    (is (= "perro" (pp/parse pp "perro")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "ratón")))))

(deftest flatten-parser
  (let [pp (pp/flatten [(pp/or "perro" "gato" "león")
                        (pp/or "_" " " "-" "/")
                        (pp/or "hambriento" "cansado" "feliz")])]
    (is (= "gato feliz" (pp/parse pp "gato feliz!!!")))))

(deftest and-parser
  (let [pp (pp/as-parser ["foo" (pp/and "bar")])
        stream (in/make-stream "foobar")
        result (r/actual-result (pp/parse-on pp stream))]
    (is (= ["foo" "bar"] result))
    (is (= 3 (in/position stream)))))

(deftest end-parser
  (let [pp (pp/end "foo")]
    (is (= "foo" (pp/parse pp "foo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"End of input expected"
                          (pp/parse pp "foobar")))))

(deftest repeating-parser-star
  (let [pp (pp/star "foo")]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (empty? (pp/parse pp "")))))

(deftest repeating-parser-plus
  (let [pp (pp/plus "foo")]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "bar")))))

(deftest repeating-parser-times
  (let [pp (pp/times "foo" 3)]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "foofoo")))))

(deftest repeating-parser-min
  (let [pp (pp/end (pp/min "foo" 3))]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (= ["foo" "foo" "foo" "foo"] (pp/parse pp "foofoofoofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Literal '\w+' expected"
                          (pp/parse pp "foofoo")))))

(deftest repeating-parser-max
  (let [pp (pp/end (pp/max "foo" 3))]
    (is (= ["foo" "foo" "foo"] (pp/parse pp "foofoofoo")))
    (is (= ["foo" "foo"] (pp/parse pp "foofoo")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"End of input expected"
                          (pp/parse pp "foofoofoofoo")))))

(deftest not-parser
  (let [pp (pp/not \a)]
    (is (nil? (pp/parse pp "b")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #".+"
                          (pp/parse pp "a")))))

(deftest not-parser-does-not-consume-the-stream
  (let [stream (in/make-stream "b")
        pp (pp/not \a)
        result (pp/parse-on pp stream)]
    (is (zero? (in/position stream)))))

(deftest optional-parser
  (let [pp (pp/optional "foo")]
    (is (= "foo" (pp/parse pp "foo")))
    (is (nil? (pp/parse pp "bar")))))

(deftest token-parser
  (let [pp (pp/token "foo")
        token (pp/parse pp "foo")]
    (is (= 0 (t/start token)))
    (is (= 3 (t/count token)))
    (is (= "foo" (t/parsed-value token)))
    (is (= "foo" (t/input-value token)))))

(deftest action-parser-with-one-arg
  (let [pp (pp/transform (pp/token "foo")
                         (fn [token] (str/upper-case (t/input-value token))))]
    (is (= "FOO" (pp/parse pp "foo")))))

(deftest action-parser-with-n-args
  (let [pp (pp/transform [(pp/token "foo")
                          (pp/token "bar")
                          (pp/token "baz")]
                         (fn [[t1 t2 t3]]
                           (format "%s -> %s -> %s"
                                   (t/input-value t1)
                                   (t/input-value t2)
                                   (t/input-value t3))))]
    (is (= "foo -> bar -> baz" (pp/parse pp "foobarbaz")))))

(deftest predicate-parser
  (let [pp (pp/predicate (fn [chr] (some? (str/index-of "aeiouAEIOU" chr)))
                         "Vowel expected")]
    (is (pp/matches? pp "a"))
    (is (not (pp/matches? pp "b")))))

(deftest case-insensitive
  (let [pp (pp/flatten [(pp/case-insensitive \f)
                        (pp/case-insensitive \o)
                        (pp/case-insensitive \o)
                        (pp/case-insensitive \!)])]
    (is (= "Foo!" (pp/parse pp "Foo!")))
    (is (= "FOO!" (pp/parse pp "FOO!")))
    (is (= "foo!" (pp/parse pp "foo!"))))
  (let [pp (pp/case-insensitive "foo!")]
    (is (= "Foo!" (pp/parse pp "Foo!")))
    (is (= "FOO!" (pp/parse pp "FOO!")))
    (is (= "foo!" (pp/parse pp "foo!")))))

(deftest a-more-complete-parser
  (let [pp (pp/transform [(pp/or (pp/case-insensitive "foo")
                                 (pp/case-insensitive "bar"))
                          (pp/plus pp/space)
                          (pp/flatten (pp/plus pp/digit))
                          (pp/optional "!")]
                         (fn [[word _ num _]]
                           [(str/lower-case word) (read-string num)]))]
    (is (= ["foo" 4] (pp/parse pp "Foo 4!")))
    (is (= ["bar" 432] (pp/parse pp "BAR      432")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "Baz 56")))))

(deftest greedy-repeating-parser-plus
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/plus-greedy pp/word
                                                              (pp/or (pp/case-insensitive "upper")
                                                                     (pp/case-insensitive "lower"))))
                                  (pp/flatten (pp/or (pp/case-insensitive "upper")
                                                     (pp/case-insensitive "lower")))])
                         (fn [[word case*]]
                           (condp = (str/lower-case case*)
                             "lower" (str/lower-case word)
                             "upper" (str/upper-case word)
                             "WAT")))]
    (is (= "abcupperlowerupper" (pp/parse pp "abcupperLowerUPPERlower")))
    (is (= "ABC" (pp/parse pp "abcupper")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "upper")))))

(deftest greedy-repeating-parser-star
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/star-greedy pp/word
                                                              (pp/or (pp/case-insensitive "upper")
                                                                     (pp/case-insensitive "lower"))))
                                  (pp/flatten (pp/or (pp/case-insensitive "upper")
                                                     (pp/case-insensitive "lower")))])
                         (fn [[word case*]]
                           (condp = (str/lower-case case*)
                             "lower" (str/lower-case word)
                             "upper" (str/upper-case word)
                             "WAT")))]
    (is (= "abcupperlowerupper" (pp/parse pp "abcupperLowerUPPERlower")))
    (is (= "ABC" (pp/parse pp "abcupper")))
    (is (= "" (pp/parse pp "upper")))))

(deftest lazy-repeating-parser-plus
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/plus-lazy pp/any
                                                            (pp/or (pp/case-insensitive "upper")
                                                                   (pp/case-insensitive "lower"))))
                                  (pp/flatten (pp/or (pp/case-insensitive "upper")
                                                     (pp/case-insensitive "lower")))])
                         (fn [[word case*]]
                           (condp = (str/lower-case case*)
                             "lower" (str/lower-case word)
                             "upper" (str/upper-case word)
                             "WAT")))]
    (is (= " abc! " (pp/parse pp " abc! LOWER")))
    (is (= "ABC" (pp/parse pp "abcupper")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "upper")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abcupperLOWER")))))

(deftest lazy-repeating-parser-star
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/star-lazy pp/any
                                                            (pp/or (pp/case-insensitive "upper")
                                                                   (pp/case-insensitive "lower"))))
                                  (pp/flatten (pp/or (pp/case-insensitive "upper")
                                                     (pp/case-insensitive "lower")))])
                         (fn [[word case*]]
                           (condp = (str/lower-case case*)
                             "lower" (str/lower-case word)
                             "upper" (str/upper-case word)
                             "WAT")))]
    (is (= " abc! " (pp/parse pp " abc! LOWER")))
    (is (= "ABC" (pp/parse pp "abcupper")))
    (is (= "" (pp/parse pp "upper")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abcupperlower")))))

(deftest negate-parser
  (let [pp (pp/seq (pp/negate pp/digit)
                   pp/digit)]
    (is (pp/matches? pp "a2"))
    (is (pp/matches? pp "?2"))
    (is (not (pp/matches? pp "22")))
    (is (not (pp/matches? pp "2a")))))

(deftest trimming-parser
  (let [pp (pp/trim \+ pp/space)]
    (is (= \+ (pp/parse pp "\t\n\r + \r\n\t")))
    (is (= \+ (pp/parse pp "+"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp " . "))))))

(deftest greedy-repeating-parser-min
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/min-greedy pp/any
                                                             3
                                                             (pp/case-insensitive "END")))
                                  (pp/case-insensitive "END")])
                         first)]
    (is (= "abc" (pp/parse pp "abcEND")))
    (is (= "abcdef" (pp/parse pp "abcdefEND")))
    (is (= "end" (pp/parse pp "endEND")))
    (is (= "abcend" (pp/parse pp "abcendEND")))
    (is (= "ENDabc" (pp/parse pp "ENDabcEND")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abc")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "END")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abend")))))

(deftest greedy-repeating-parser-max
  (let [pp (pp/transform (pp/end [(pp/flatten (pp/max-greedy pp/any
                                                             3
                                                             (pp/case-insensitive "END")))
                                  (pp/case-insensitive "END")])
                         first)]
    (is (= "abc" (pp/parse pp "abcEND")))
    (is (= "a" (pp/parse pp "aEND")))
    (is (= "" (pp/parse pp "END")))
    (is (= "end" (pp/parse pp "endEND")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abcendEND")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "ENDabcEND")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "abc")))))

(deftest separated-by
  (let [pp (pp/end (pp/separated-by pp/digit \,))]
    (is (= [\3 \, \4 \, \5]
           (pp/parse pp "3,4,5")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pp/parse pp "3,4,5,")))))

(deftest separated-by-2
  (let [identifier (pp/or (pp/transform [\[
                                         (-> (pp/predicate #(and (not= \[ %)
                                                                 (not= \] %))
                                                           "Any except square brackets expected")
                                             pp/plus
                                             pp/flatten)
                                         \]]
                                        (fn [[left body right]] body))
                          (pp/flatten (pp/plus (pp/or pp/word \_))))
        ws pp/space
        pp (pp/separated-by [(pp/star ws)
                             identifier
                             (pp/star ws)
                             (pp/or (pp/case-insensitive "ASC")
                                    (pp/case-insensitive "DESC"))
                             (pp/star ws)]
                            \,)]
    (is (pp/matches? pp "[id] ASC"))
    (is (pp/matches? pp "\n	[id] ASC, [model] desc"))
    (is (pp/matches? pp "[id]ASC,[model]desc"))))

(comment
 (re-find #"Literal '\s' expected" "Literal 'a' expected")
 (re-find #"Literal '" "Literal 'a' expected")
 (= (seq [\a \b \c]) [\a \b \c])
 (str/index-of "Richo" \a)
 (read-string "42")

 ,)
