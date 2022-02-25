(ns aoc.day4
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io])
  (:import java.lang.Integer))

(def win-row '("X" "X" "X" "X" "X"))

(defn update-list
  ([lst itm nv]
    (map (fn [x] (if (= x itm) nv x)) lst)))

(defn check-board
  [board number]
  (map #(update-list  % number "X") board))

(defn win?
  [board]
  (if (seq (filter #(= win-row %) board))
    true
    (loop [column 0]
      (if (< column 5)
        (if (= win-row (map #(nth % column) board))
          true
          (recur (inc column)))
        false))))

(defn get-boards
  [input]
  (loop [lines input
         boards '()]
   (if (empty? lines)
     boards
     (recur (drop 6 lines) (conj boards (map #(string/split (string/trim %) #" +") (rest (take 6 lines))))))))

(defn row-sum
  [row]
  (if (= row win-row)
    0
    (loop [members row
           sum 0]
      (if (empty? members)
        sum
        (let [current (first members)]
          (if (= current "X")
            (recur (rest members) sum)
            (recur (rest members) (+ sum (Integer/parseInt current)))))))))

(defn get-score
  [board]
  (reduce + (map row-sum board)))

(defn get-first-winning-score
  [input]
  (let [input-file (line-seq (io/reader input))]
    (loop [numbers (string/split (first input-file) #",")
           boards (get-boards (rest input-file))
           last 0]
     (if-let [win-boards (seq (filter win? boards))]
       (* last (get-score (first win-boards)))
       (recur (rest numbers) (map #(check-board % (first numbers)) boards) (Integer/parseInt (first numbers)))))))

(defn get-last-winning-score
  [input]
  (let [input-file (line-seq (io/reader input))]
    (loop [numbers (string/split (first input-file) #",")
           boards (get-boards (rest input-file))
           last-number 0
           scores nil]
      (if (and (seq numbers) (seq boards))
        (if-let [win-board (first (filter win? boards))]
          (recur (rest numbers) (map #(check-board % (first numbers))(remove win? boards)) (Integer/parseInt (first numbers)) (conj scores (* last-number (get-score win-board))))
          (recur (rest numbers) (map #(check-board % (first numbers)) boards) (Integer/parseInt (first numbers)) scores))
        (first scores)))))

;;(get-last-winning-score "/Users/tyler/aoc2021/inputs/day4.txt")




