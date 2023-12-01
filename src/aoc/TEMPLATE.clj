(ns aoc.dayTEMPLATE
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 99)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [[_ xa xb ya yb] (re-matches
                         #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)"
                         str-input)
        f #(Integer/parseInt %)
        ]
  {:x1 (f xa) :y1 (f ya) :x2 (f xb) :y2 (f yb)}))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(t/are [i o] (= i o)
       [1 1])


(defn part1 [input]
  (pprint input))

; (part1 input)
; (println (time (part1 input)))



(defn part2 [input]
  (pprint input))

; (prn (time (part2 input)))
; (part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
