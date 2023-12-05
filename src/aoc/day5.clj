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

; (defn to-map [{ :keys [dest source len]}]
;   (into (sorted-map) (map vector  (range source (+ source len)) (range dest (+ dest len))))
;  )

; ; (to-map { :dest 52 :source 50 :len 48 })

; (defn map-to-hash [{:keys [ranges]}]
;   (apply merge (map to-map ranges)))
; ; (pp/pprint (map-to-hash (first (:maps t1))))

; (defn lookup [i m]
;   (let [r (m i)]
;     (if (nil? r)
;       i
;       r)
;     ))

(defn range-map [i { :keys [dest source len]}]
  (let [ls source
        hs (+ source len)
        diff (- dest source)
        ]
    ; (println diff i ls hs)
    ; (println (and (>= i ls) (<= i hs)))
    (if (and (>= i ls) (<= i hs))
      (reduced (+ i diff)) 
      i
      )
    ))

; (-> 13
;   (range-map { :dest 50 :source 98 :len 2 })
;   (range-map { :dest 52 :source 50 :len 48 })
;     )
; (range-map { :dest 52 :source 50 :len 48 })

(defn run-range-map [i ranges]
  (reduce range-map i ranges))

; (reduce range-map 79 (first tm))
(run-range-map 79 (first tm))

(def tm (map :ranges (:maps t1)) )
(pp/pprint tm)
; (first tm)

(defn trace-seed [ranges seed]
  (reduce run-range-map seed ranges))

; (trace-seed tm 79)
; (trace-seed tm 14)
; (trace-seed tm 55)
; (trace-seed tm 13)

(defn part1 [input]
  (let [seeds (:seeds input)
        rs (map :ranges (:maps input)) 
        locations (map #(trace-seed rs %) seeds)]
    (reduce min ##Inf locations))
  )

; (part1 t1)
; (pp/pprint input)
; (part1 input)
; (println (time (part1 input)))

(defn seeds-from-ranges [in]
  (let [rs (partition 2 in)
        rs' (map #(range (first %) (+ (first %) (second %)) ) rs)
        ]
    (apply concat rs')
    ))

; (seeds-from-ranges [2 7 9 11])
; ( seeds-from-ranges (:seeds t1))

(defn part2 [input]
  (let [seeds-rs (:seeds input)
        seeds (seeds-from-ranges seeds-rs)
        rs (map :ranges (:maps input)) 
        locations (map #(trace-seed rs %) seeds)]
    (reduce min ##Inf locations))
  )

; (part2 t1)
; (part2 input)
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
