(ns aoc.day7
  (:require [clojure.string :as string])
  (:import java.lang.Integer))

(defn get-positions
  [input]
  (map #(Integer/parseInt %) (string/split (string/trim (slurp input)) #",")))

(defn calculate-fuel-old
  [positions location]
  (reduce + (map (fn [x] (if (> location x) (- location x) (- x location))) positions)))

(defn align-lowest-fuel-old
  [positions]
  (let [ max-pos (apply max positions)
         min-pos (apply min positions)]
    (apply min (map #(calculate-fuel-old positions %) (range min-pos (inc max-pos))))))

(defn solve-1
  [input]
  (align-lowest-fuel-old (get-positions input)))

;;(solve-1 "/Users/tyler/aoc2021/inputs/day7.txt")

(defn calculate-single-fuel
  [p1 dest]
  (if (= p1 dest)
    0
    (let [ min-pos (min p1 dest)
           max-pos (max p1 dest)]
      (loop [ current min-pos
              fuel 0]
        (if (< current max-pos)
          (recur (inc current) (+ fuel (- max-pos current)))
          fuel)))))

(defn tst-fuel
  [p1 location]
  (let [diff (Math/abs (- location p1))]
    (* diff (int (/ (inc diff) 2)))))

(defn calculate-fuel
  [positions location]
  (reduce + (pmap #(calculate-single-fuel % location) positions)))

(defn align-lowest-fuel
  [positions]
  (apply min (pmap #(calculate-fuel positions %) (range (apply min positions) (inc (apply max positions))))))

(defn solve-2
  [input]
  (align-lowest-fuel (get-positions input)))

(solve-2 "/Users/tyler/aoc2021/inputs/day7.txt")

