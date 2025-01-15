(ns day6.solution2
  (:require
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn get-input
  "Gathers the input into a tuple of the following format:
  [point y-length x-length board]
  "
  []
  (loop [y 0
         remaining-lines (str/split (slurp "src/day6/input.txt") #"\n")
         guard-position nil
         board []]
    (let [next-line (first remaining-lines)]
      (if (nil? next-line)
        [guard-position        ; The initial start of the of guard (\^ char)
         (count board)         ; The height of the board
         (count (nth board 0)) ; The width of the board
         (to-array-2d board)]  ; The board itself
        (let [guard-x-index (str/index-of next-line "^")
              line (str/split next-line #"")]
          (recur
           (inc y)
           (rest remaining-lines)
           (if guard-x-index [y guard-x-index] guard-position)
           (conj board line)))))))

(defn outside-board?
  [y-length x-length [y x]]
  (or (<= y-length y) (<= x-length x)))

(defn traversed?
  [board [y x]]
  (= (aget board y x) "X"))

(defn obstacle-in-same-direction?
  [board [y x] direction]
  (let [item (aget board y x)]
    (or
     (and (= item "U") (= direction :up))
     (and (= item "D") (= direction :down))
     (and (= item "L") (= direction :left))
     (and (= item "R") (= direction :right)))))

(defn obstacle?
  [board [y x]]
  (let [item (aget board y x)]
    (or
     (= item "#")
     (= item "U")
     (= item "D")
     (= item "L")
     (= item "R"))))

(defn get-next-step
  [board [y x] direction]
  (condp = direction
    :right (if (obstacle? board [y (inc x)])
             [[y x] :down]
             [[y (inc x)] :right])

    :down (if (obstacle? board [(inc y) x])
            [[y x] :left]
            [[(inc y) x] :down])

    :left (if (obstacle? board [y (dec x)])
            [[y x] :up]
            [[y (dec x)] :left])

    :up (if (obstacle? board [(dec y) x])
          [[y x] :right]
          [[(dec y) x] :up])

    (throw (Exception. "Incorrect direction"))))

(defn traverse-board
  [input]
  (let [[guard-position y-length x-length board] input]
    (loop [position guard-position
           direction :up
           placements []]
      (if (or (nil? position) (outside-board? y-length x-length position))
        placements
        (let [[next-position next-direction] (try
                                               (get-next-step board position direction)
                                               (catch ArrayIndexOutOfBoundsException _
                                                 [nil direction]))
              position-traversed (traversed? board position)
              [pos-y pos-x] position
              _ (aset board pos-y pos-x "X")]
          (recur
           next-position
           next-direction
           (if position-traversed placements (conj placements [pos-y pos-x]))))))))

(defn main
  "The solution for https://adventofcode.com/2024/day/5 part 2"
  []
  (let [[guard-position y-length x-length board] (get-input)
        viable-placements (rest (traverse-board [guard-position y-length x-length board]))]
    viable-placements))

(comment
  ; scratch
  (def input (get-input))

  ; solution
  (def placements (main))

  ;
  )
