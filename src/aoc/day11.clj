(ns aoc.day11
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


(def day 11)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn to-map [lines]
  (reduce-kv
    (fn [acc r line]
     (merge
      acc
        (reduce-kv
          (fn [acc2 c ch] (if (= ch \#) (assoc acc2 [r c] :star) acc2))
          acc
          (vec line)
      )))
   {}
   lines))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       ]
    (to-map lines)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn find-empty-rows [in]
  (let [rs (map first (keys in))
        max-r (reduce max rs)]
    (remove (set rs) (range 0 (inc max-r)))
    ))

(defn find-empty-cols [in]
  (let [cs (map second (keys in))
        max-c (reduce max cs)]
    (remove (set cs) (range 0 (inc max-c)))
    ))

; (find-empty-rows t1)
; (find-empty-cols t1)
(defn remap-pos [in]
  (let [er (find-empty-rows in)
        ec (find-empty-cols in)]
    (into {}
      (map
        (fn [[[r c] v]]
           (let [rinc (count (filter #(< % r) er))
                 cinc (count (filter #(< % c) ec))
                 ]
             [[(+ r rinc) (+ c cinc)] v]
             )
             )
           in)
          )))

; (pp/pprint (remap-pos t1))

(defn manhattan [a b]
  (sum (map #(abs (- %1 %2)) a b)))

; (manhattan [1 1] [ 7 3])

(defn dists [in]
  (let [stars (sort (vec (keys in))) ]
    (map-indexed (fn [i s] (map #(manhattan s %) (drop (inc i) stars))) stars)
    ))

; (pp/pprint (t1))
; (sum (flatten (dists (remap-pos t1))))
; (map (fn [s] (map (fn [e] ) (keys t1))) (keys t1))

(defn part1 [input]
  (sum (apply concat (dists (remap-pos input)) )))

; (part1 test-input)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)


(defn remap-pos [dist in]
  (let [er (find-empty-rows in)
        ec (find-empty-cols in)]
    (into {}
      (map
        (fn [[[r c] v]]
           (let [rinc (count (filter #(< % r) er))
                 cinc (count (filter #(< % c) ec))
                 ]
             [[(+ r (* (dec dist) rinc)) (+ c (* (dec dist) cinc))] v])
             )
         in))))

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
