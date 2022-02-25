(ns aoc.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn construct-map
  [path]
  (loop [input (line-seq (io/reader path))
         heights (vector)]
    (if (seq input)
      (recur (rest input) (conj heights (into [] (map int (char-array (first input))))))
      heights)))

(defn risk-of-row
  [y heights]
  (if (= y 0)
    (let [x (nth (nth heights y) 0)
          x2 (nth (nth heights y) 1)
          y2 (nth (nth heights (inc y)) 0)]
      )))

(defn risk-of
  [x y heights]
  (let [pt (nth (nth heights y) x)]
    (if (= y 0)
      (if ()))))

(defn get-total-risk-level
  ([height-map]
    (if (empty? height-map)
      0
      (let [current (first (first height-map))]
        (if (and (< current (second (first height-map)))
                 (< current (first (second height-map)))
                 (< current (second (second height-map))))
          (get-total-risk-level height-map 1 0 (- current 47))
          (get-total-risk-level height-map 1 0 0)))))
  ([height-map x y total]
    (if (< x (count (first height-map)))
      (recur height-map (inc x) y (+ total (risk-of x y height-map)))
      (if (< y (count height-map))
        (recur height-map (inc ))))))
