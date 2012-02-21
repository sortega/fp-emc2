(ns billboards.core
  (:require [clojure.java.io :as io])
  (:use [clojure.contrib.seq-utils :only [indexed]]))

(defn take-line [cols words]
  (loop [line  []
         len   0
         words words]
    (if-let [w (first words)]
      (let [space    (if (zero? len) 0 1)
            next-len (+ len space (count w))]
        (if (> next-len cols)
          [line words]
          (recur (conj line w)
                 next-len
                 (rest words))))
      [line nil])))

(defn make-para [cols words]
  (let [take-cols (partial take-line cols)]
    (->> (iterate
           (comp take-cols second)
           [nil words])
      rest
      (map first)
      (take-while seq))))

(defn max-font [w h text]
  (let [chars (count text)
        words (re-seq #"\S+" text)
        max-word-size (apply max (map count words))
        max-f (min
                (int (quot w max-word-size))
                (int (Math/sqrt (/ (* w h) chars))))]
    (loop [f max-f]
      (when (pos? f)
        (let [cols (quot w f)
              rows (quot h f)
              para (make-para cols words)]
          (cond
            (nil? para)            nil
            (<= (count para) rows) f
            :else                  (recur (dec f))))))))

(defn -main [input-file]
  (binding [*in* (io/reader input-file)]
    (let [cases (Integer/parseInt (read-line) 10)]
      (doseq [[lineno line] (indexed (line-seq *in*))]
        (let [[_ w h text] (re-matches #"(\d+) (\d+) (.*)" line)]
          (println (str "Case #" (inc lineno) ": "
                     (max-font
                       (Integer/parseInt w)
                       (Integer/parseInt h)
                       text))))))))
