(ns day2.solution
  (:require [clojure.string :as str]))

(defn remove-index [arr idx]
  (vec (concat (subvec arr 0 idx) (subvec arr (inc idx)))))

(defn get-list []
  (let [file-contents (slurp "src/day2/input.txt")
        ; Split contents by newline
        file-contents-newline-split (str/split file-contents #"\n")
        ; Split each line by the spaces between to create list of strings
        file-contents-space-split (mapv #(str/split %1 #"\s+") file-contents-newline-split)
        ; Parse the strings, to return a number list
        file-contents-space-split-parsed (mapv #(mapv parse-long %1) file-contents-space-split)]
    file-contents-space-split-parsed))

(defn is-safe [xs]
  (let [comparison-tuples (partition 2 1 xs)
        safe-range-results (every?
                            (fn [[left right]]
                              (let [distance (abs (- left right))]
                                (and (>= distance 1) (<= distance 3))))
                            comparison-tuples)]
    safe-range-results))

(defn process-levels-1 [xs]
  (cond
    (apply < xs) (is-safe xs)
    (apply > xs) (is-safe xs)
    :else false))

(defn process-levels-2 [xs]
  (let [possibilities (reduce
                       (fn [accum value] (conj accum (remove-index xs value)))
                       [xs]
                       (range (count xs)))]
    (some #(cond
            (apply < %1) (is-safe %1)
            (apply > %1) (is-safe %1)
            :else false)
          possibilities)))

(defn process-reports-1 [reports]
  (reduce
   (fn [accum xs] (if (process-levels-1 xs)
                    (inc accum)
                    accum))
   0
   reports))

(defn process-reports-2 [reports]
  (reduce
   (fn [accum xs] (if (process-levels-2 xs)
                    (inc accum)
                    accum))
   0
   reports))

(defn main-1
  "The solution for https://adventofcode.com/2024/day/2 part 1"
  []
  (let [lst (get-list)]
    (process-reports-1 lst)))

(defn main-2
  "The solution for https://adventofcode.com/2024/day/2 part 2"
  []
  (let [lst (get-list)]
    (process-reports-2 lst)))

(comment
  ; helpers
  (get-list)
  (main-1)
  (main-2)

  (partition 2 2 [1 2 3 4 5 6 7 8 9 10])
  (count [1 2 4])
  (= 1 1)
  ;
  )

