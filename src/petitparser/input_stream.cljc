(ns petitparser.input-stream
  (:refer-clojure :exclude [peek]))

(defn make-stream [src]
  {:src src :pos (atom 0)})

(defn position ^long [{pos :pos}] @pos)
(defn source [stream] (:src stream))

(defn reset-position! [stream pos]
  (reset! (:pos stream) pos)
  nil)

(defn peek [{:keys [src pos]}]
  (nth src @pos nil))

(defn next! [stream]
  (when-let [val (peek stream)]
    (swap! (:pos stream) inc)
    val))

(defn end? [stream]
  (nil? (peek stream)))

(defn take-seq [stream ^long count]
  (lazy-seq
   (if-not (or (= 0 count)
               (end? stream))
     (cons (next! stream)
           (take-seq stream (dec count))))))

(defn take! [stream count]
  (apply str (take-seq stream count)))
