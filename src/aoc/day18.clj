(ns aoc.day18
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.math.numeric-tower :as ma]  
   )
  (:use aoc.core))


(def day 18)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

; R 6 (#70c710)
(defn prepare-line [s]
  (let [[_ dir n color] (re-matches #"([RLDU]) (\d+) \(\#(.*)\)" s)]
    { :dir (keyword dir) :steps (parse-long n) :color color}
    ))

(prepare-line "R 4 (#87ab87)")

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input) ]
    (mapv prepare-line lines)
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))
(def t1 test-input)
; (pp/pprint t1)

(def start [0 0])

(range 10 5 -1)
(defn steping [[x y] dir steps]
  (case dir
    :R (mapv (fn [x] [x y]) (range (inc x) (+ x steps 1)))
    :L (mapv (fn [x] [x y]) (range (dec x) (- x steps 1) -1))
    :D (mapv (fn [y] [x y]) (range y (+ y steps 1)))
    :U (mapv (fn [y] [x y]) (range (dec y) (- y steps 1) -1))
    ))

; (steps [0 0] :R 6)
; (steps [10 0] :L 6)
; (steps [0 0] :D 6)
; (steps [0 10] :U 6)

(defn build-path [steps]
  (reduce
   (fn [path { :keys [dir steps]}]
     (into path (steping (last path) dir steps))
     )
   [start] steps))

(count (set (build-path t1)))

(defn get-neigh [[x y]]
  [
   [(dec x) y]
   [(inc x) y]
   [x (inc y)]
   [x (inc y)]
   [(dec x) (dec y)]
   [(inc x) (dec y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]
   ])

(defn fill [st queue]
  (let [n (peek queue)]
    ; (println queue)
    (cond
      (nil? n) st
      (st n) (recur st (pop queue))
      :default (recur (conj st n) (into (pop queue) (get-neigh n)))
      )
    ))

; (count (set (fill (set (build-path t1))  [[1 1]])))
; (fill (set (build-path t1))  [[1 1]])

(defn part1 [input]
  (count (set (fill (set (build-path input))  [[1 1]])))
  )

; (pp/pprint (map hash1 input))

; (part1 t1)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)


(defn to-dir [c]
  (case c
    \0 :R
    \1 :D
    \2 :L
    \3 :U
    ))

(defn input-to-part2 [{ color :color }]
  {
   :dir (to-dir (last color))
   :steps (Long/parseLong (s/join (drop-last color)) 16)
   }
  )

(pp/pprint t1)
(defn step2 [[x y] dir steps]
  (case dir
    :R [(+ x steps) y]
    :L [(- x steps) y]
    :D [x (+ y steps)]
    :U [x (- y steps)]
    ))

; (step2 [15 0] :L 10)

(defn build-points [steps]
  (reduce
   (fn [path { :keys [dir steps]}]
     (conj path (step2 (last path) dir steps))
     )
   [start] steps))

; (build-points t1)
(m/columns (m/transpose (m/matrix [[1 2] [3 4]])) )

; (partition 2 1 [1 2 3 4])
(defn point-pairs [ps] (partition 2 1 (concat ps [(first ps)])))

(defn to-col-m [ps]
  (m/transpose (m/matrix ps)))

(m/columns (to-col-m [[1 6] [3 1]]))

(defn count-area [points]
  (/
   (sum
      (map #(m/det (to-col-m %)) (point-pairs points))
    )
   2
   )
  )

(count-area [[1 6] [3 1] [7 2] [4 4] [8 5]])

(pp/pprint (build-points t1))
(count-area (build-points t1))
(count-area (build-points (map input-to-part2 t1)))

; (pp/pprint (mapv input-to-part2 t1))

(defn part2 [input]
  (reduce max
          (map #(traverse-and-count input %)  (generate-starts input))
   ))

; (part2 t1)
(part2 input)
; (println (time (part2 input)))


(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    ; (println (part2 input))
    ))

(solve-problem (mk-input-filename day))
