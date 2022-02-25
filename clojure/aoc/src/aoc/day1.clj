(ns aoc.day1
  (:require [clojure.string :as string])
  (:import java.lang.Integer))

;; Solution 1:
(defn num-prev-larger
  "Takes in a path to a file of numbers, and returns the number of
   entries that are larger than the preceding entry"
  ([input]
    (let [depths (map #(Integer/parseInt %) (string/split (slurp input) #"\n"))]
      (num-prev-larger (first depths) (rest depths) 0)))
  ([prev depths deeper]
    (if (seq depths)
      (if (< prev (first depths))
        (recur (first depths) (rest depths) (inc deeper))
        (recur (first depths) (rest depths) deeper))
      deeper)))

;; Solution 2:
(defn num-sum-larger
  "Takes in a path to a file of numbers, and returns the number of
   groups of three that are larger than the preceding group of
   three in the file."
  ([input]
    (let [depths (map #(Integer/parseInt %) (string/split (slurp input) #"\n"))]
      (num-sum-larger (+ (first depths) (second depths) (nth depths 2)) (rest depths) 0)))
  ([prev depths larger]
    (if (> (count depths) 2)
      (let [current (+ (first depths) (second depths) (nth depths 2))]
        (if (< prev current)
          (recur current (rest depths) (inc larger))
          (recur current (rest depths) larger)))
      larger)))

