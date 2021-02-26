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

(defn take-while-seq [pred stream]
  (lazy-seq
   (if-let [next (peek stream)]
     (if (pred next)
       (cons (next! stream)
             (take-while-seq pred stream))))))

(defn take-while! [pred stream]
  (apply str (take-while-seq pred stream)))

(defn count-while! [pred stream]
  (loop [n 0]
    (let [next (peek stream)]
      (if (and next (pred next))
        (do
          (next! stream)
          (recur (inc n)))
        n))))
