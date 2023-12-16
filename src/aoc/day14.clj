(ns aoc.day14
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


(def day 14)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (def to-kw { \# :damaged \. :operational \? :unknown})

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       ]
    ; (println lines)
    (mapv (fn [l] (vec l)) lines)
    ))
; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn transpose [m]
    (apply mapv vector m))

(def t2 (transpose t1))
; (pp/pprint t2)

(defn roll-part [part]
  (if (every? #(= % \#) part)
    part
    (vec (reverse (sort part)) )))
; (roll-part [\. \O \. \. \O])
; (roll-part [\# \# \# \# \#])

(defn roll-col-north [col]
  (->> col
    (partition-by #(= % \#))
    (map roll-part)
    (apply concat)
    (vec)
       ))

; (roll-col-north [\. \O \. \. \. \# \O \. \. \O])
; (roll-col-north [\# \. \# \. \. \O \# \. \# \#] )

(defn roll-all-north [img]
  (as-> img l
      (transpose l)
      ; (println)
      (mapv roll-col-north l)
      ; (println l)
      (transpose l)
      ))
; (pp/pprint (roll-all-north t1))
(defn count-col [col]
  (let [mx (count col)]
    (reduce-kv (fn [s i v] (if (= v \O) (+ s (- mx i)) s)) 0 col)
    ))
; (count-col  [\O \O \O \. \. \# \# \O \. \.])

(defn count-points [img]
  (as-> img l
    (transpose l)
    (map count-col l)
    (sum l)
      ))


(defn part1 [input]
  (count-points (roll-all-north input)))

; (part1 t1)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)

; Correct but slow
(defn roll-cycle [in]
  (as-> in l
    ;north
    (transpose l)
    (mapv roll-col-north l)
    (transpose l)

    ;west
    (mapv roll-col-north l)

    ;south
    (transpose l)
    (mapv reverse l)
    (mapv roll-col-north l)
    (mapv reverse l)
    (transpose l)

    ;east
    (mapv reverse l)
    (mapv roll-col-north l)
    (mapv reverse l)
    )
  )

; (pp/pprint (roll-cycle t1))
; (pp/pprint (roll-cycle (roll-cycle t1)) )
; (pp/pprint (nth (iterate roll-cycle t1) 5))

(defn get-steady-state [in]
  (reduce #(if (= %1 %2) (reduced %1) %2) (iterate roll-cycle in)))

; (pp/pprint (get-steady-state t1))
; (= '(1 2) '(1 2))
; (take 5 (iterate inc 0))


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
