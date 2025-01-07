(ns day4.solution
  (:require [clojure.string :as str]))

(defn get-input []
  (let [file-contents (slurp "src/day4/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")
        character-lines (mapv #(str/split %1 #"") file-contents-newline-split)]
    character-lines))

(defn right-indexes        [[x y] length] (map (fn [value] [(+ x value) y]) (range length)))
(defn bottom-right-indexes [[x y] length] (map (fn [value] [(+ x value) (+ y value)]) (range length)))
(defn bottom-indexes       [[x y] length] (map (fn [value] [x (+ y value)]) (range length)))
(defn bottom-left-indexes  [[x y] length] (map (fn [value] [(- x value) (+ y value)]) (range length)))
(defn left-indexes         [[x y] length] (map (fn [value] [(- x value) y]) (range length)))
(defn top-left-indexes     [[x y] length] (map (fn [value] [(- x value) (- y value)]) (range length)))
(defn top-indexes          [[x y] length] (map (fn [value] [x (- y value)]) (range length)))
(defn top-right-indexes    [[x y] length] (map (fn [value] [(+ x value) (- y value)]) (range length)))

(defn right-diagonal-indexes [[x y] _] [[(dec x) (inc y)] [x y] [(inc x) (dec y)]])
(defn left-diagonal-indexes  [[x y] _] [[(dec x) (dec y)] [x y] [(inc x) (inc y)]])

(def index-fns-1
  [right-indexes
   bottom-right-indexes
   bottom-indexes
   bottom-left-indexes
   left-indexes
   top-left-indexes
   top-indexes
   top-right-indexes])

(def index-fns-2
  [right-diagonal-indexes
   left-diagonal-indexes])

(defn extract-str [input points]
  (str/join "" (map #(get-in input %1) points)))

(defn count-radially [input point length input-index-fns eq-fn]
  (reduce
   (fn [accum index-fn]
     (let [points (index-fn point length)
           str-output (extract-str input points)]
       (if (eq-fn str-output)
         (inc accum)
         accum)))
   0
   input-index-fns))

(defn main-1
  "The solution for https://adventofcode.com/2024/day/4 part 1"
  []
  (let [input (get-input)
        length-x (count (get input 0))
        length-y (count input)]
    (reduce
     (fn [accum point]
       (+ accum (count-radially input point 4 index-fns-1 #(= "XMAS" %1))))
     0
     (for [y (range length-y)
           x (range length-x)]
       [x y]))))

(defn main-2
  "The solution for https://adventofcode.com/2024/day/4 part 2"
  []
  (let [input (get-input)
        length-x (count (get input 0))
        length-y (count input)]
    (reduce
     (fn [accum point]
       (let [output (count-radially input point nil index-fns-2 #(or (= "MAS" %1) (= "SAM" %1)))]
         (if (= output 2)
           (inc accum)
           accum)))
     0
     (for [y (range length-y)
           x (range length-x)]
       [x y]))))

(comment
  (def input (get-input))

  ; utility functions
  (extract-str input (right-indexes [0 0] 4))
  (extract-str input (bottom-right-indexes [0 0] 4))
  (extract-str input (bottom-indexes [0 0] 4))
  (extract-str input (bottom-left-indexes [0 0] 4))
  (extract-str input (left-indexes [0 0] 4))
  (extract-str input (top-left-indexes [0 0] 4))
  (extract-str input (top-indexes [0 0] 4))
  (extract-str input (top-right-indexes [0 0] 4))

  (extract-str input (right-diagonal-indexes [1 1] nil))
  (extract-str input (left-diagonal-indexes [1 1] nil))

  (count-radially input [0 0] 4 index-fns-1 #(= %1 "XMAS"))
  (count-radially input [0 0] 4 index-fns-2 #(= %1 "XMAS"))

  (for [y (range 10)
        x (range 10)]
    [x y])

  ; solutions
  (main-1)
  (main-2)
  ;
  )
