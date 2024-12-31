(ns day3.solution
  (:require [clojure.string :as str]))

(defn get-list []
  (let [file-contents (slurp "src/day3/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")]
    file-contents-newline-split))

(def instruction-regex-1 #"mul\((\d{1,3}),(\d{1,3})\)")
(def instruction-regex-2 #"(don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\))")

(defn main-1
  "The solution for https://adventofcode.com/2024/day/3 part 1"
  []
  (let [strs (get-list)]
    (reduce
     (fn [accum value]
       (->>
        ; Create lazy list of regex matches
        (re-seq instruction-regex-1 value)
        ; For each match, parse, multiply, add to accumulator
        (reduce
         (fn [acc [_instr & nums]]
           (let [[x y] (map parse-long nums)]
             (+ acc (* x y))))
         accum)))
     0
     strs)))

(defn main-2
  "The solution for https://adventofcode.com/2024/day/3 part 2"
  []
  (let [strs (get-list)
        enabled (atom true)]
    (reduce
     (fn [accum value]
       (->>
        ; Create lazy list of regex matches
        (re-seq instruction-regex-2 value)
        ; For each match, parse, multiply, add to accumulator if enabled
        (reduce (fn [acc [instr _instr2 & nums]]
                  (cond
                    (= instr "don't()") (do (reset! enabled false) acc)
                    (= instr "do()") (do (reset! enabled true) acc)
                    @enabled (let [[x y] (map parse-long nums)]
                               (+ acc (* x y)))
                    :else acc))
                accum)))
     0
     strs)))

(comment
  (get-list)
  (re-seq instruction-regex-1 "mul(109,129)lasdkjflkasjdfkmul(909,230)")
  (re-seq instruction-regex-2 "don't()mul(109,129)lasdkjflkdo()asjdfkmul(909,230)")
  (re-seq instruction-regex-2 "")

  (main-1)
  (main-2)
  ;
  )


