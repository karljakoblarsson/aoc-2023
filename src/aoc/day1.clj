(ns aoc.day1
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

(def day 1)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn prepare-input [str-input]
  (map #(Integer/parseInt %) (s/split-lines str-input)
   ))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(t/are [i o] (= i o)
       [1 1])

(defn findSumTo [in]
  (let [comple (reduce (fn [acc n] (assoc acc (- 2020 n) n)) {} in)
        ]
    (reduce-kv (fn [acc k v] (let [cand (get comple v)] (if (nil? cand) nil (reduced [v cand]) ))) nil comple)
    )
  )

(findSumTo t1)

(defn part1 [input]
  (let [[a b] (findSumTo input)] (* a b)))

; (part1 input)
; (println (time (part1 input)))

; Stupid brute-force solution.
(defn cart [a b c ]
  (for [ae a be b ce c] [ae be ce])
  )

(cart t1 t1 t1)

(defn find-sum-to-three [n in]
  (first
    
  (filter #(= n (sum %)) (cart in in in))
    )
  )

(find-sum-to-three 2020 t1)


(defn part2 [input]
  (apply * (find-sum-to-three 2020 input)))

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
