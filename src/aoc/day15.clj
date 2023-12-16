(ns aoc.day15
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   [clojure.math.numeric-tower :as ma]
   )
  (:use aoc.core))


(def day 15)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (def to-kw { \# :damaged \. :operational \? :unknown})
; (println testfile)

(defn prepare-input [str-input]
  (let [str-input' (s/replace str-input #"\n" "")
        lines (s/split str-input' #",")
       ]
    ; (println lines)
    lines
    ))
; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn hash1 [s]
  (reduce
   (fn [cv c]
     (-> cv
         (+ (int c))
         (* 17)
         (mod 256)
      ))
   0
   s)
  )

(hash1 "HASH")

(t/are [i o] (= (hash1 i) o)
  "HASH" 52
  "rn=1" 30
  "cm-" 253
  "qp=3" 97
  "cm=2" 47
  "qp-" 14
  "pc=4" 180
  "ot=9" 9
  "ab=5" 197
  "pc-" 48
  "pc=6" 214
  "ot=7" 231
)

(defn part1 [input]
  (sum (map hash1 input)))

; (pp/pprint (map hash1 input))

(part1 t1)
(part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)

(defn part2 [input]
  (count-points
   ; Waaay to slow
    (nth (iterate roll-cycle input) 1000000000)
   ))

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
