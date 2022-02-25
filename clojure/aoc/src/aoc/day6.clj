(ns aoc.day6
  (:require [clojure.string :as string])
  (:import java.lang.Integer))

(defn opening-state [input]
  (let [fish (string/split (string/trim (slurp input)) #","x0)]
    (loop [nums '("1" "2" "3" "4" "5" "6" "7" "8")
            state {:0 0 :1 0 :2 0 :3 0 :4 0 :5 0 :6 0 :7 0 :8 0}]
      (if (seq nums)
        (recur (rest nums) (assoc state (keyword (first nums)) (count (filter #(= % (first nums)) fish))))
        state))))

(defn next-day
  [state]
  {:0 (:1 state) :1 (:2 state) :2 (:3 state) :3 (:4 state) :4 (:5 state) :5 (:6 state) :6 (+ (:0 state) (:7 state)) :7 (:8 state) :8 (:0 state)})

(defn total-after
  [input days]
  (loop [ state (opening-state input)
          remaining-days days]
    (if (> remaining-days 0)
      (recur (next-day state) (dec remaining-days))
      (reduce + (vals state)))))

;;(total-after "/Users/tyler/aoc2021/inputs/day6.txt" 256)




