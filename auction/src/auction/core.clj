(ns auction.core
  (:require [clojure.java.io :as io]))
  ;(:use [clojure.contrib.seq-utils :only [indexed]]))

(defn random-seq 
  "Pseudorandom generator with seed p_1 and recurrence 
   p_{i+1} = ((a * p_i + b) (mod m)) + 1"
  [p1 a b m]
  (iterate
    (fn [p] (inc (mod (+ (* a p) b) m)))
    p1))
  
(defn product-seq
  "Pseudorandom product sequence"
  [p1 w1 m k a b c d]
  (map vector (random-seq p1 a b m)
              (random-seq w1 c d k)))

(defn vector-order
  "Defines a partial order for a vector given several scalar orders"
  [& scalar-orders]
  (fn [a b]
    (every?
      identity
      (map apply 
           scalar-orders
           (map list a b)))))
(def plt (vector-order < >))
(def ple (vector-order <= >=))
(def pgt (vector-order > <))
(def pge (vector-order >= <=))

(defn most-preferred
  "Given a partial order of preference, update a group of favorites with a
   new element"
  [porder favs elem]
  (let [worse-than-elem  #(porder elem %),
        better-than-elem #(porder % elem),
        {discarded true,
         kept      false} (group-by worse-than-elem favs)]
    (if (not-any? better-than-elem favs)
        (conj kept elem)
        kept)))
(def pmin (partial most-preferred plt))
(def pmax (partial most-preferred pgt))

(defn auction
  "Tuple [terribles bargains] for an auction"
  [n p1 w1 m k a b c d]
  (let [products (take n (product-seq p1 w1 m k a b c d))
        terribles (reduce pmax [] products)
        bargains  (reduce pmin [] products)]
    (map count [terribles bargains])))

(defn parse-input [lines]
  (let [parse-line (fn [line] (map #(Integer/parseInt %) (re-seq #"\d+" line))),
        cases (Integer/parseInt (first lines) 10)]
    (map parse-line (rest lines))))

(defn print-case [n [terribles bargains]]
  (println (str "Case #" (inc n) ": " terribles " " bargains)))

(defn auction-ui [input]
  (dorun 
    (keep-indexed 
      (fn [nline params]
        (print-case nline (apply auction params)))
      (parse-input (line-seq (io/reader input))))))

(defn -main
  ([] (auction-ui *in*))
  ([input-file] (auction-ui input-file)))
