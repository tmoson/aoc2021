(ns aoc.day5redo
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import java.lang.Integer))


(defn get-directions
  [input]
  (map
    (fn [x] (map (fn [y] (map #(Integer/parseInt %) (string/split y #","))) x))
    (map #(string/split % #" -> ")
        (line-seq (io/reader input)))))

(defn get-slope
  [x1 y1 x2 y2]
  (/ (- y2 y1) (- x2 x1)))

(defn get-b
  [x y slope]
  (- y (* slope x)))

(defn draw-diagonal
  [mp [x1 y1] [x2 y2]]
  (let [slope (get-slope x1 y1 x2 y2)
        b (get-b x1 y1 slope)
        xs (if (> x2 x1)
             (range x1 x2)
             (range x1 x2 -1))
        ys (map #(+ b (* slope %)) xs)]
    (loop [pts (map vector xs ys)
           new-mp mp]
      (if-let [current {:x (first (first pts)) :y (second (first pts))}]
        (if (get new-mp current)
          (recur (rest pts) (update new-mp current inc))
          (recur (rest pts) (assoc new-mp current 1)))
        new-mp))))

(defn draw-straight-line
  [mp min-x max-x min-y max-y]
  (loop [x min-x
        y min-y
        new-mp mp]
    (if (and (<= max-x x) (<= max-y y))
      (if (get new-mp {:x x :y y})
       (recur (inc x) (inc y) (update new-mp {:x x :y y} inc))
       (recur (inc x) (inc y) (assoc new-mp {:x x :y y} 1)))
      (if (<= max-x x)
        (if (get new-mp {:x x :y y})
          (recur (inc x) y (update new-mp {:x x :y y} inc))
          (recur (inc x) y (assoc new-mp {:x x :y y} 1)))
        new-mp))))

(defn no-diagonals
  [input]
  (loop [lines (filter #(or (= (first (first %)) (first (second %))) (= (second (first %)) (second (second %)))) (get-directions input))
         sea-map '{}]
    (if (seq lines)
      (let [line (first lines)
            pt1 (first line) pt2 (second line)
            max-x (max (first pt1) (first pt2))
            min-x (min (first pt1) (first pt2))
            max-y (max (second pt1) (second pt2))
            min-y (min (second pt1) (second pt2))]
        (recur (rest lines) (draw-straight-line sea-map min-x max-x min-y max-y)))
      (count (filter #(> % 1) sea-map)))))

(no-diagonals "/Users/tyler/aoc2021/inputs/day5.txt")

