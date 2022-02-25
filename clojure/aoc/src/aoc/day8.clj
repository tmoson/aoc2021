(ns aoc.day8
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader]]
            [clojure.set :as cset])
  (:import java.lang.Integer))


(defn split-input-output
  [filename]
  (map (fn [x] (map (fn [y] (string/split y #" ")) (string/split x #" \| " ))) (line-seq (reader filename))))

(defn get-output
  [input-output]
  (map second input-output))

(defn get-input
  [input-output]
  (map first input-output))

(defn unique?
  [input]
  (or
    (= (count input) 2)
    (= (count input) 3)
    (= (count input) 4)
    (= (count input) 7)))

(defn solve-1
  [input]
  (reduce + (map #(count (filter unique? %)) (get-output (split-input-output input)))))

(defn get-one
  [line]
  (first (filter #(= (count %) 2) line)))

(defn get-four
  [line]
  (first (filter #(= (count %) 4) line)))

(defn get-seven
  [line]
  (first (filter #(= (count %) 3) line)))

(defn get-eight
  [line]
  (first (filter #(= (count %) 7) line)))

(defn get-nine
  [four seven others]
  (first (filter #(= (count (cset/intersection % (cset/union four seven))) 5) (filter #(= (count %) 6) others))))

(defn get-five
  [four seven others]
  (first (filter #(and (= (count (cset/intersection % seven)) 2) (= (count (cset/intersection % four)) 3)) (filter #(= (count %) 5) others))))

(defn get-six
  [five one others]
  (first (filter #(and (= (count (cset/intersection % one)) 1) (= (count (cset/intersection % five)) 5)) (filter #(= (count %) 6) others))))

(defn get-two
  [five others]
  (first (filter #(= (count (cset/intersection % five)) 3) (filter #(= (count %) 5) others))))

(defn get-three
  [five others]
  (first (filter #(= (count (cset/intersection % five)) 4) (filter #(= (count %) 5) others))))

(defn get-zero
  [one five others]
  (first (filter #(and (= (count (cset/intersection % one)) 2) (= (count (cset/intersection % five)) 4)) (filter #(= (count %) 6) others))))

(defn num-list-to-map
  [num-list]
  (loop [nums num-list
         place 0
         ret-val (hash-map)]
    (if (empty? nums)
      ret-val
      (recur (rest nums) (inc place) (assoc ret-val (keyword (reduce str (first nums))) place)))))

(defn str-to-num
  ([num-list num-string]
    (if (= (set num-string) (first num-list))
      0
      (str-to-num num-list (set num-string) 1)))
  ([num-list num-set ind]
    (println num-set)
    (if (= num-set (nth num-list ind))
      ind
      (recur num-list num-set (inc ind)))))

(defn convert-output
  [num-list output]
  (loop [to-convert output
         acc ""]
    (if (seq to-convert)
      (recur (rest to-convert) (str acc (str-to-num num-list (first to-convert))))
      acc)))

(defn process-line
  [line]
  (let [line-set (map set (first line))
        one (set (get-one (first line)))
        four (set (get-four (first line)))
        seven (set (get-seven (first line)))
        eight (set (get-eight (first line)))
        nine (get-nine four seven line-set)
        five (get-five four seven line-set)
        six (get-six five one line-set)
        two (get-two five line-set)
        three (get-three five line-set)
        zero (get-zero one five line-set)
        output (second line)]
    (println "Zero: " zero)
    (println "One: " one)
    (println "Two: " two)
    (println "Three: " three)
    (println "Four: " four)
    (println "Five: " five)
    (println "Six: " six)
    (println "Seven: " seven)
    (println "Eight: " eight)
    (println "Nine: " nine)
    (Integer/parseInt
      (convert-output (list zero one two three four five six seven eight nine) output))))

;; (defn solve
;;   [input]
;;   (let [input-output (split-input-output)
;;         inputs (get-input input-output)
;;         outputs (get-output input-output)]
;;     ))

(defn output-total
  [input]
  (reduce + (map process-line (split-input-output input))))


;; (def sample (split-input-output "/Users/tyler/aoc2021/inputs/day8Test.txt"))
;; (def first-line (process-line (first sample)))
;; (println first-line)
;; (def second-line (process-line (second sample)))
;; (println second-line)
;; (def third-line (process-line (nth sample 2)))
;; (println third-line)
;; (println)
;; (println)
;; (println (nth sample 3))
;; ;;(def fourth-line (nth sample 3))
;; ;;(def count-of-6 (filter #(= (count %) 6) (first (nth sample 3))))
;; ;;(println count-of-6)

;; (def fourth-line (process-line (nth sample 3)))
;; fourth-line

(output-total "/Users/tyler/aoc2021/inputs/day8.txt")

;;(process-line first-line)




