(ns aoc.day2
  (:require [clojure.string :as string])
  (:import java.lang.Integer))

(defmacro parse-command
  [command]
  `(string/split ~command #" "))

;; ===== Solution 1 =====
;; ======================
(defn old-move
  [[direction amt] [x y]]
  (cond
    (= direction "forward")
      (vector (+ x (Integer/parseInt amt)) y)
    (= direction "down")
      (vector x (+ y (Integer/parseInt amt)))
    :default
      (vector x (- y (Integer/parseInt amt)))))

(defn old-drive
  "Move from 0,0 to wherever your destination is, and return the product"
  ([input]
    (let [directions (string/split (slurp input) #"\n")]
      (if (empty? directions)
        0
        (drive (rest directions) (old-move (parse-command (first directions)) '[0 0])))))
  ([directions location]
    (if (empty? directions)
      (reduce * location)
      (recur (rest directions) (old-move (parse-command (first directions)) location)))))


;; ===== Solution 2 =====
;; ======================
(defn move
  [[direction amt] [aim [x y]]]
  (cond
    (= direction "down")
      (vector (+ aim (Integer/parseInt amt)) (vector x y))
    (= direction "up")
      (vector (- aim (Integer/parseInt amt)) (vector x y))
    :default
      (let [units (Integer/parseInt amt)]
        (vector aim (vector (+ x units) (+ y (* aim units)))))))

(defn drive  
  "Move from 0,0 to wherever your destination is, and return the product"
  ([input]
    (let [directions (string/split (slurp input) #"\n")]
      (if (empty? directions)
        0
        (drive (rest directions) (move (parse-command (first directions)) '[0 [0 0]])))))
  ([directions location]
    (if (empty? directions)
      (reduce * (second location))
      (recur (rest directions) (move (parse-command (first directions)) location)))))

(drive "/Users/tyler/aoc2021/inputs/day2Test.txt")


