(ns petitparser.results)

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
                 (throw (ex-info (str message " at " position)
                                 {:position position}))))

(defn success [result]
  (ParseSuccess. result))

(defn failure [position message]
  (ParseFailure. position message))
