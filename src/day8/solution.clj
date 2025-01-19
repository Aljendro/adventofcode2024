(ns day8.solution
  (:require
   [clojure.string :as str]))

(defn add-to-map [m k v]
  (update m k (fnil conj []) v))

(defn get-input []
  (let [file-contents (slurp "src/day8/input.txt")
        file-contents-newline-split (str/split file-contents #"\n")
        parse-contents (mapv #(str/split % #"") file-contents-newline-split)]
    [parse-contents (count parse-contents) (count (get parse-contents 0 []))]))

(defn is-signal?
  "Check if the character is a single lowercase letter, uppercase letter, or digit."
  [character]
  (boolean (re-matches #"[a-zA-Z0-9]" (str character))))

(defn gather-signals
  "Get all the locations of the signals into a map of signal->location array"
  [rows columns input]
  (loop [i 0
         j 0
         locations {}]
    (cond
      (>= i rows) locations
      (>= j columns) (recur (inc i) 0 locations)
      :else (let [position-char (get-in input [i j])]
              (if (is-signal? position-char)
                (recur i (inc j) (add-to-map locations position-char [i j]))
                (recur i (inc j) locations))))))

(defn is-inside-board?
  "Check if a position is inside the board"
  [rows columns [x y]]
  (boolean (and (<= 0 x (dec rows)) (<= 0 y (dec columns)))))

(defn antinode-positions
  "Get the antinode positions as defined in https://adventofcode.com/2024/day/8"
  [rows columns [x1 y1] [x2 y2]]
  (let [[x-translate y-translate] [(- x2 x1) (- y2 y1)]]
    (loop [[innerx innery] [x2 y2]
           positions [[x1 y1] [x2 y2]]]
      (let [new-position [(+ innerx x-translate) (+ innery y-translate)]]
        (if (not (is-inside-board? rows columns new-position))
          positions
          (recur new-position (conj positions new-position)))))))

(defn get-signal-antinode-positions
  "Gather all antinodes for a signal"
  [rows columns signal-positions]
  (let [len (count signal-positions)]
    (loop [i 0
           j 0
           antinodes '()]
      (cond
        (>= i len) antinodes
        (>= j len) (recur (inc i) 0 antinodes)
        (= i j) (recur i (inc j) antinodes)
        :else (recur i
                     (inc j)
                     (concat antinodes
                             (antinode-positions
                              rows
                              columns
                              (get signal-positions i)
                              (get signal-positions j))))))))

(defn combine-all-antinode-positions
  "For all signals gather the antinode positions"
  [rows columns signals]
  (reduce
   (fn [acc [_ v]]
     (concat acc (get-signal-antinode-positions rows columns v)))
   '()
   signals))

(defn main
  "Solution for day 8"
  []
  (let [[input rows columns] (get-input)
        signals (gather-signals rows columns input)
        antinode-positions (combine-all-antinode-positions rows columns signals)
        uniq-valid-antinode-positions (distinct antinode-positions)]
    uniq-valid-antinode-positions))

(comment
  ; utility
  (is-signal? "A")
  (is-signal? ".")
  (is-signal? "a")

  (def in (get-input))
  (def positions (get in 0))
  (def rows (get in 1))
  (def columns (get in 2))

  (antinode-positions rows columns [0 1] [0 0])

  (is-inside-board? rows columns [0 2])

  (def signals (apply gather-signals in))

  (get-signal-antinode-positions rows columns (get signals "T"))
  (def all-antinode-positions (combine-all-antinode-positions signals 0 0))

  (count (filter (partial is-inside-board? (get in 1) (get in 2)) all-antinode-positions))

  ; solution
  (count (main))
  ;
  )
