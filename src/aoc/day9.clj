(ns aoc.day9
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

(def day 9)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))



(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       ]
    (mapv parse-array lines)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(def t2 (last t1))

(defn diffs [arr]
  (mapv - (rest arr) arr))

(defn to-zero [arr]
  (take-while (fn [a] (not (every? #(= % 0) a))) (iterate diffs arr)) )

; (to-zero t2)

(defn find-n [tzs]
  (let [lasts (mapv last tzs)]
    (reduce  +  0 lasts)
    ))

; (find-n (to-zero t2))


(defn part1 [input]
  (let [vs (map #(find-n (to-zero %)) input) ]
    (sum vs)
  ))

; (part1 t1)
; (part1 t2)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println t1)

(defn find-n-start [tzs]
  (let [lasts (mapv first tzs)]
    (reductions #(- %2 %1) 0 (reverse lasts) )
    ))

; (find-n-start (to-zero t2))
; (part2 t1)

(defn part2 [input]
  (let [vs (map #(find-n-start (to-zero %)) input) ]
    ; (println vs)
    (sum (map last vs))
  ))

; (while)

; (part2 t1)
; (part2 input)
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
