(ns aoc.day12
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


(def day 12)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(def to-kw { \# :damaged \. :operational \? :unknown})

(defn parse-line [s]
  (let [[springs conditions] (s/split s #" ")]
    { :springs (mapv to-kw springs)
      :conds (parse-array conditions)
     }))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       ]
    (mapv parse-line lines)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn pattern [n]
  (mapv #(= \1 %) (Integer/toString n 2)))

; (pattern 4)

(defn patterns [n]
  (mapv (fn [m]
    (mapv
       #(= \1 %)
       (format (str "%" n "s") (Integer/toString m 2))
           ))
    (range 0 (ma/expt 2 n)))
  )
; (patterns 3)
(pp/pprint (:springs (first t1)) )

(def t2
  { :springs [:damaged :operational :damaged :operational :damaged :damaged :damaged],
    :conds [1 1 3]
   })

(defn is-valid [springs conds]
  (->> springs
    (partition-by identity)
    (filter (fn [v] (every? #(= % :damaged) v)))
    (mapv count)
    (= conds)
       ))
; (is-valid t2)

(defn replace-p [springs is ps]
    (reduce-kv
      (fn [acc x i] (assoc acc i (if (nth ps x) :operational :damaged)))
      springs
      is))
; (replace-p (:springs t2) [0 2 4 5 6] [true false true false false false])
; (replace-p
;   [:unknown :unknown :unknown :operational :damaged :damaged :damaged]
;   [0 2 4 5 6]
;   [true false true false false false])
; (replace-p
;   [:unknown :unknown :unknown :operational :damaged :damaged :damaged]
;   (0 1 2)
;  [false false false])

; (into [5 6] [2 3])
(defn gen-perm [springs]
  (let [no-damaged (count (filter #(= % :unknown) springs))
        damaged-indices (remove nil? (map-indexed #(if (= %2 :unknown) %1 nil) springs))
        ps (patterns no-damaged)]
    ; (println damaged-indices ps)
    (map
      #(replace-p springs (vec damaged-indices ) %)
      ps)
    ))
; (pp/pprint (gen-perm [:unknown :unknown :unknown :operational :damaged :damaged :damaged]))

; (filter #(is-valid % [1 1 3]) (gen-perm [:unknown :unknown :unknown :operational :damaged :damaged :damaged]))

(defn count-valid [{ :keys [springs conds]}]
  (count (filter #(is-valid % conds) (gen-perm springs))))

; (count-valid (second t1))

(defn part1 [input]
  (sum (map count-valid input)))

; (part1 t1)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)

(defn part2 [input]
  (sum (apply concat (dists (remap-pos 1000000 input)))))

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
