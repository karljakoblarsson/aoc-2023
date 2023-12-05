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
; (run-range-map 79 (first tm))

(def tm (map :ranges (:maps t1)) )
; (pp/pprint tm)
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

(defn seeds-to-r [in]
  (let [rs (partition 2 in)
        rs' (map (fn [[s l]] { :start s :len l}) rs)
        ]
    rs'
    ))

(defn is-inside [i a b]
  (and (>= i a) (<= i b)))


(defn map-range [{ alen :len as :start } { blen :len  dest :dest bs :source }]
  (let [aend (+ as alen)
        bend (+ bs blen)
        diff (- dest bs)
        ]
    (cond
      ; a inside b
      (and (is-inside as bs bend) (is-inside aend bs bend))
      [
      { :start (+ as diff) :len alen } ; inside
       ]

      ; b inside a
      (and (is-inside bs as aend) (is-inside bend as aend))
      [
      { :start (+ bs diff) :len blen } ; inside
      { :start as :len (- bs as) } ; before b
      { :start (inc bend)  :len (- aend (inc bend)) } ; after b
       ]

      ; start of b inside end of a
      (and (is-inside bs as aend) (not (is-inside bend as aend)) )
      [
      { :start (+ bs diff) :len (- aend bs) } ; inside
      { :start as :len (- bs as) } ; before b
       ]

      ; start of a inside end of b
      (and (is-inside as bs bend) (not (is-inside aend bs bend)) )
      [
      { :start (+ as diff) :len (- bend as) } ; inside
      { :start (inc bend)  :len (- aend (inc bend)) } ; after b
       ]

      :default nil
      )
    ))
; (part2 t1)

; (map-range { :start 79 :len 14} { :dest 52 :source 50 :len 48 })

; (-> 13
;   (range-map { :dest 50 :source 98 :len 2 })
;   (range-map { :dest 52 :source 50 :len 48 })
;     )
; (range-map { :dest 52 :source 50 :len 48 })

(defn run-map-range [i ranges]
  (let [nx (remove nil?
            (map #(map-range i %) ranges)
                  )
        ]
  ; (println "rmr " i nx)
    (if (empty? nx) i (flatten nx))
    ))

; (run-map-range { :start 79 :len 14} (first tm))
(defn r-m-rs [is ranges]
  ; (println "rs" is)
  (flatten (map #(run-map-range % ranges) is)))

; (println (r-m-rs [{ :start 79 :len 14}]  (first tm)))
; (r-m-rs '({ :start 81 :len 14}) (second tm))
; (println (reduce r-m-rs [{ :start 79 :len 14}] tm))

(defn trace-range [mappings start-range]
  (reduce r-m-rs [start-range] mappings))

; (trace-range tm {:start 79 :len 0}) ; 82
; (trace-range tm {:start 55 :len 0}) ; 86
; (trace-range tm {:start 14 :len 0}) ; 43
; (trace-range tm {:start 13 :len 0}) ; 35

; (trace-range tm {:start 81 :len 1}) ; 46

; (trace-range tm {:start 79 :len 12}) ; 46
; (trace-range tm {:start 79 :len 13}) ; 60

; (trace-range tm {:start 79 :len 14 }) ; 60

(defn part2 [input]
  (let [seeds-rs (:seeds input)
        seeds (seeds-to-r seeds-rs)
        mappings (map :ranges (:maps input)) 
        locations (map #(trace-range mappings %) seeds)
        locs (flatten locations)
        locs' (filter (fn [e] (> (e :len) 0)) locs)
        ]
    ; (pp/pprint locs)
    (reduce min ##Inf (map :start locs')))
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
