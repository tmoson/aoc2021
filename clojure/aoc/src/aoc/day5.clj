(ns aoc.day5
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import java.lang.Integer))

(defn fill-horizontal-line
  [mp [x1 y1] [x2 y2]]
  (if (< x1 x2)
    (loop [row (vec (nth mp y1))
           current x1]
     (if (<= current x2)
       (recur (assoc row current (inc (nth row current))) (inc current))
       (assoc mp y1 row)))
    (loop [row (vec (nth mp y1))
           current x2]
     (if (<= current x1)
       (recur (assoc row current (inc (nth row current))) (inc current))
       (assoc mp y1 row)))))

(defn fill-vertical-line
  [mp [x1 y1] [x2 y2]]
  (if (< y1 y2)
    (loop [row (vec (nth mp y1))
           current y1
           new-map mp]
     (if (<= current y2)
       (recur (nth new-map (inc current)) (inc current) (assoc new-map current (assoc row x1 (inc (nth row x1)))))
       new-map))
    (loop [row (vec (nth mp y1))
         current y2
         new-map mp]
    (if (<= current y1)
      (recur (nth new-map (inc current)) (inc current) (assoc new-map current (assoc row x1 (inc (nth row x1)))))
      new-map))))

(defn get-directions
  [input]
  (map
    (fn [x]
      (map
        (fn [y]
          (map #(Integer/parseInt %) (string/split y #","))) x))
    (map #(string/split % #" -> ")
        (line-seq (io/reader input)))))

(defn no-diagonal-intersections
  [input]
  (let [directions (get-directions input)
        mp (vec (repeat 1001 (vec (repeat 1001 0))))]
    (loop [lines (rest directions)
           pt1 (first (first directions))
           pt2 (second (first directions))
           current-map mp]
      (if (seq lines)
        (cond
          (= (first pt1) (first pt2))
            (recur (rest lines) (first (first lines)) (second (first lines)) (fill-vertical-line current-map pt1 pt2))
          (= (second pt1) (second pt2))
            (recur (rest lines) (first (first lines)) (second (first lines)) (fill-horizontal-line current-map pt1 pt2))
          :default
            (recur (rest lines) (first (first lines)) (second (first lines)) current-map))
        current-map))))

(defn num-intersecting-no-diagonals
  [input]
  (loop [lines (no-diagonal-intersections input)
         intersecting 0]
    (if (seq lines)
      (recur (rest lines) (+ intersecting (count (filter #(> % 1) (first lines)))))
      intersecting)))




;; ============= NOT MY CODE, TRYING TO FIX MINE ============ ;;
(defn calc-slope [coord]
   (/ (- (:y2 coord) (:y1 coord)) (- (:x2 coord) (:x1 coord))))

 (defn calc-b [coord]
   (let [slope (calc-slope coord)]
      (- (:y1 coord) (* slope (:x1 coord)))))

 (defn mk-straight-line [minx maxx miny maxy]
   (for [x (range minx (inc maxx))
         y (range miny (inc maxy))]
     {:x x :y y}))

 (defn mk-diagonal-line [coord]
   (let [slope (calc-slope coord)
         b (calc-b coord)
         step (if (> (:x2 coord) (:x1 coord)) 1 -1)
         xs (if (= step 1)
              (range (:x1 coord) (inc (:x2 coord)) step)
              (range (:x1 coord) (dec (:x2 coord)) step))
         ys (map #(+ b (* slope %)) xs)]
     (for [pt (map vector xs ys)]
       {:x (first pt) :y (second pt)})))

 (defn coord->line [coord]
   (let [minx (min (:x1 coord) (:x2 coord))
         maxx (max (:x1 coord) (:x2 coord))
         miny (min (:y1 coord) (:y2 coord))
         maxy (max (:y1 coord) (:y2 coord))]
     (if (or (= 0 (- maxy miny))
             (= 0 (- maxx minx)))
       (mk-straight-line minx maxx miny maxy)
       (mk-diagonal-line coord))))

 (defn add-to-grid [mp coord]
   (if (get mp coord)
     (update mp coord inc)
     (assoc mp coord 1)))

 (defn straight-line-p [coord]
   (or (= (:x1 coord) (:x2 coord))
       (= (:y1 coord) (:y2 coord))))

 (defn input->coords [input]
   (let [string->pair (fn [strv] (mapcat #(string/split % #",") (string/split strv #" -> ")))
         coords->map #(zipmap [:x1 :y1 :x2 :y2] %)]
     (->> (string/split-lines input)
          (map string->pair)
          (map #(map (fn [v] (Long/parseLong v)) %))
          (map coords->map))))

 (defn solve [input filter-fn]
   (->> input
        input->coords          ; convert input to coords {:x1 :y1 :x2 :y2}
        (filter filter-fn)     ; filter by filter-fn - only relevant for part1
        (map #(coord->line %)) ; convert input coords to all coords along the line
        flatten                ; flatten list of lists to single list
        (reduce #(add-to-grid %1 %2) {}) ; update hash-map with coords and inc count
        (map second)                     ; take the count from hash-map
        (filter #(> % 1))                ; filter if count cross > 1
        count))

;; ============== END OF NOT MY CODE ==============

(solve (slurp "/Users/tyler/aoc2021/inputs/day5.txt") straight-line-p)

(num-intersecting-no-diagonals "/Users/tyler/aoc2021/inputs/day5.txt")


;;(println (get-directions "/Users/tyler/aoc2021/inputs/day5Test.txt"))

