(ns aoc.day6
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

(def day 6)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [[tl dl] (s/split-lines str-input)
        times (parse-array (s/replace tl #"Time: " ""))
        dists (parse-array (s/replace dl #"Distance: " ""))
        ]
    (mapv (fn [t d] { :time t :dist d}) times dists)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn calc-dist [[h r]]
  (* h r))

(defn possibilities [t]
  (map calc-dist (map (fn [x] [x (- t x)]) (range 1 t))))

; (possibilities 7)

(defn count-beating [{ t :time d :dist }]
  (count (filter #(> % d) (possibilities t))))

; (count-beating (second t1))
; (possibilities (:time (second t1)))

(defn part1 [input]
  (let [beatings (map count-beating input)
        ]
    (reduce * 1 beatings)
  ))

; (part1 t1)
; (pp/pprint input)
; (part1 input)
; (println (time (part1 input)))

; (println t1)

(defn part2 [input]
  (let [ts (map :time input)
        t (parse-int (s/join ts))
        ds (map :dist input)
        d (parse-int (s/join ds))
        ]
    (count-beating { :time t :dist d})
    )
  )

; (part2 t1)
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
