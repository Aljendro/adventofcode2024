(ns day5.solution1
  (:require
   [common]
   [clojure.string :as str]
   [clojure.math :as math]))

(defn gen-graph
  "Builds node->set(node)

  The nodes and the set of nodes they point to"
  [ordering-section]
  (reduce (fn [accum value]
            (let [[x y] (map #(parse-long %1) (str/split value #"\|"))]
              (update accum x (fnil conj #{}) y)))
          {}
          ordering-section))

(defn get-input []
  (let [file-contents (slurp "src/day5/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")
        [ordering-section [_empty-str & update-section]] (split-with #(not= % "") file-contents-newline-split)
        graph (gen-graph ordering-section)
        parsed-updated-section (map #(mapv parse-long (str/split %1 #",")) update-section)]
    [graph parsed-updated-section]))

(defn order-okay?
  "Returns true when left can come before right in a list"
  [graph left right]
  (let [set-value (get-in graph [right left])]
    (not set-value)))

(defn is-valid-list?
  "Returns true when the ordering of all values satisfies ordering from input"
  [graph arr]
  (let [last-index (dec (count arr))]
    (loop [x 0
           y 1]
      (cond
        (>= x last-index) true
        (> y last-index) (recur (+ x 1) (+ x 2))
        (order-okay? graph (nth arr x) (nth arr y)) (recur x (+ y 1))
        :else false))))

(defn filter-valid-arrays
  "Returns a arrays that are valid"
  [graph arrays]
  (filter (fn [lst] (is-valid-list? graph lst)) arrays))

(defn get-middle-element
  "Return the middle element of a array"
  [arr]
  (let [arr-length (count arr)
        middle-index (int (math/floor (/ arr-length 2)))]
    (get arr middle-index)))

(defn add-middle-elements
  [lsts]
  (reduce
   (fn [accum arr]
     (+ accum (get-middle-element arr)))
   0
   lsts))

(defn main
  "The solution for https://adventofcode.com/2024/day/5 part 1"
  []
  (let [[graph update-section] (get-input)
        valid-arrays-lst (filter-valid-arrays graph update-section)
        final-value (add-middle-elements valid-arrays-lst)]
    (println final-value)))

(comment
  (def input (get-input))

  (get input 1)
  ; Utility functions
  (order-okay? (get input 0) 94 24)
  (order-okay? (get input 0) 94 6546546545)
  (int (math/floor (/ 5 2)))

  ; solutions
  (main)
  ;
  )
