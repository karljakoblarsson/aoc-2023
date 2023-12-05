(ns aoc.day5
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

(def day 5)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn parse-range [s]
  (let [[dest source len] (parse-array s)]
    { :dest dest :source source :len len }
    ))

(defn parse-map [lines]
  (let [n (s/replace (first lines) #" map:" "")
        ms (map parse-range (rest lines))]
    { :name n :ranges ms }
    ))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        parts (partition-by-empty-line lines)
        seeds-str (-> parts (first) (first))
        seeds (parse-array (s/replace seeds-str #"seeds: " ""))
        maps (map parse-map (rest parts))
        ]
    { :seeds  seeds
      :maps maps }
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn to-map [{ :keys [dest source len]}]
  (into (sorted-map) (map vector  (range source (+ source len)) (range dest (+ dest len))))
 )

; (to-map { :dest 52 :source 50 :len 48 })

(defn map-to-hash [{:keys [ranges]}]
  (apply merge (map to-map ranges)))
; (pp/pprint (map-to-hash (first (:maps t1))))

(defn lookup [i m]
  (let [r (m i)]
    (if (nil? r)
      i
      r)
    ))

(def tm (map map-to-hash (:maps t1)))
; (pp/pprint tm)

(defn trace-seed [list-of-maps seed]
  (reduce lookup seed list-of-maps))

; (trace-seed tm 79)
; (trace-seed tm 14)
; (trace-seed tm 55)
; (trace-seed tm 13)

(defn part1 [input]
  (let [seeds (:seeds input)
        ms (:maps input)
        ms' (map map-to-hash ms)

        locations (map #(trace-seed ms' %) seeds)]
    (reduce min ##Inf locations))
  )

; (part1 t1)
; (pp/pprint input)
; (part1 input)
; (println (time (part1 input)))


(defn part2 [plays]
  (let [wins (filter #(> % 0)
              (map (comp play-to-score filter-wins) plays))
        ]
    (sum (map no-cards (map :game plays)))
    )
  )

; (count t1)
; (part2 t1)

(part2 input)
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
