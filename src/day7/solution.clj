(ns day7.solution1
  (:require
   [clojure.string :as str]))

(defn parse-line [s]
  (let [[result values] (str/split s #": ")]
    {:result (parse-long result)
     :values (map parse-long (str/split values #" "))}))

(defn get-input []
  (let [file-contents (slurp "src/day7/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")
        parsed-contents (map parse-line file-contents-newline-split)]
    parsed-contents))

(defn has-solution
  [result values]
  (if (<= (count values) 1)
    (= result (first values))
    (let [[a b & rest-values] values]
      (or
       (has-solution result (conj rest-values (+ a b)))
       (has-solution result (conj rest-values (* a b)))))))

(defn has-solution-2
  [result values]
  (if (<= (count values) 1)
    (= result (first values))
    (let [[a b & rest-values] values]
      (or
       (has-solution-2 result (conj rest-values (parse-long (str a b))))
       (has-solution-2 result (conj rest-values (+ a b)))
       (has-solution-2 result (conj rest-values (* a b)))))))

(defn main
  "The solution for https://adventofcode.com/2024/day/7"
  []
  (let [input (get-input)
        lines-with-solutions (filter #(has-solution-2 (:result %) (:values %)) input)]
    (reduce #(+ %1 (:result %2)) 0 lines-with-solutions)))

(comment
  ; utility

  (has-solution 190 '(10 19))
  (has-solution-2 7290 '(6 8 6 15))
  ; solutions
  (def r (main))
  ;
  )
