(ns aoc.day3
  (:require [clojure.string :as string])
  (:import java.lang.Integer))

(defmacro pow
  [n p]
  `(reduce * (repeat ~p ~n)))

(defn max-bit-val
  [num-bits]
  (loop [remaining num-bits
         max-val 0]
    (if (= remaining 1)
      (+ max-val 1)
      (recur (dec remaining) (+ max-val (pow 2 (dec remaining)))))))

;; I've got two implementations for this...and I'm not sure which is better
;; could be that the work is just too light for either implementation to be
;; meaningfully different, or a difference in how filter works as opposed to a
;; recursive loop
(defn most-common-bit-new
  [coll]
  (let [comp-bits (map first coll)]
    (if (> (count (filter #(= % \0) comp-bits))
           (count (filter #(= % \1) comp-bits)))
      "0"
      "1")))

(defn most-common-bit
  [coll]
  (loop [comp-bits (map first coll)
         ones 0
         zeros 0]
    (if (seq comp-bits)
      (if (= (first comp-bits) \1)
        (recur (rest comp-bits) (inc ones) zeros)
        (recur (rest comp-bits) ones (inc zeros)))
      (if (> zeros ones)
        "0"
        "1"))))

(defn most-common
  [coll]
  (loop [comp coll
         ones 0
         zeros 0]
    (if (seq comp)
      (if (= (first comp) \1)
        (recur (rest comp) (inc ones) zeros)
        (recur (rest comp) ones (inc zeros)))
      (if (> zeros ones)
        \0
        \1))))

(defn least-common
  [coll]
  (loop [comp coll
         ones 0
         zeros 0]
    (if (seq comp)
      (if (= (first comp) \1)
        (recur (rest comp) (inc ones) zeros)
        (recur (rest comp) ones (inc zeros)))
      (if (> zeros ones)
        \1
        \0))))

(defn get-gamma
  ([coll]
    (get-gamma (map rest coll) (most-common-bit coll)))
  ([coll bit-string]
    (if (seq (first coll))
      (recur (map rest coll) (str bit-string (most-common-bit coll)))
      bit-string)))

(defn get-gamma-new
  ([coll]
    (get-gamma (map rest coll) (most-common-bit-new coll)))
  ([coll bit-string]
    (if (seq (first coll))
      (recur (map rest coll) (str bit-string (most-common-bit-new coll)))
      bit-string)))


(defn get-epsilon
  [gamma]
  (bit-xor (Integer/parseInt gamma 2) (max-bit-val (count gamma))))

(defn get-power-reading
  [input]
  (let [gamma (get-gamma (string/split (slurp input) #"\n"))
        epsilon (get-epsilon gamma)]
    (* (Integer/parseInt gamma 2) epsilon)))

;;(get-power-reading "/Users/tyler/aoc2021/inputs/day3.txt")

(defn get-oxygen
  [nums]
  (loop [numbers nums
         ind 0]
    (if (seq (rest numbers))
      (let [zero-or-one (most-common (map #(nth % ind) numbers))]
        (recur (filter #(= (nth % ind) zero-or-one) numbers) (inc ind)))
      (Integer/parseInt (apply str (first numbers)) 2))))

(defn get-co2
  [nums]
  (loop [numbers nums
         ind 0]
    (if (seq (rest numbers))
      (let [zero-or-one (least-common (map #(nth % ind) numbers))]
        (recur (filter #(= (nth % ind) zero-or-one) numbers) (inc ind)))
      (Integer/parseInt (first numbers) 2))))


(defn get-life-support
  [input]
  (let [readings (string/split (slurp input) #"\n")]
    (* (get-oxygen readings) (get-co2 readings))))

(get-life-support "/Users/tyler/aoc2021/inputs/day3.txt")
