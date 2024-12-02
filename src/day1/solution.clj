(ns day1.solution
  (:require [clojure.string :as str]))

(defn get-lists []
  (let [file-contents (slurp "src/day1/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")
        file-contents-left-right-tuples (map #(str/split %1 #"\s+") file-contents-newline-split)
        file-contents-left-right-tuples-parsed (map (fn [[i j]] [(Integer/parseInt i) (Integer/parseInt j)])
                                                    file-contents-left-right-tuples)
        left-list (mapv first file-contents-left-right-tuples-parsed)
        right-list (mapv second file-contents-left-right-tuples-parsed)]
    [left-list right-list]))

(defn process-lists-1 [left-list right-list]
  (let [sorted-left-list (sort left-list)
        sorted-right-list (sort right-list)
        distances (map #(Math/abs (- %1 %2)) sorted-left-list sorted-right-list)]
    (reduce + distances)))

(defn process-lists-2 [left-list right-list]
  (let [right-list-frequencies (frequencies right-list)]
    (reduce
     (fn [accum value]
       (+ accum
          (* value (get right-list-frequencies value 0))))
     0
     left-list)))

(defn main-1
  "The solution for https://adventofcode.com/2024/day/1 part 1"
  []
  ; Read the txt file from src/day1/input.txt
  ; Split the two lists into a left and right column
  ; Sort the left and right lists
  ; Get the distance between the same indexes in two lists after they are sorted
  ; Return the result of all of those values added up
  (let [[left-list right-list] (get-lists)]
    (process-lists-1 left-list right-list)))

(defn main-2
  "The solution for https://adventofcode.com/2024/day/1 part 2"
  []
  ; Read the txt file from src/day1/input.txt
  ; Split the two lists into a left and right column
  ; Sort the left and right lists
  ; Get the distance between the same indexes in two lists after they are sorted
  ; Return the result of all of those values added up
  (let [[left-list right-list] (get-lists)]
    (process-lists-2 left-list right-list)))

(comment
  ; Examples of reading in a file in clojure
  (slurp "src/day1/input.txt")
  (str/split (first *1) #"\s+")

  ; execute the helper functions
  (get-lists)
  (process-lists-1 (first *1) (second *1))
  (process-lists-2 (first *1) (second *1))
  ; solutions
  (main-1)
  (main-2)
  20520794
  ;
  )
