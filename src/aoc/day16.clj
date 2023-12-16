(ns aoc.day16
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


(def day 16)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(def char-to-kw {
  \. :empty
  \| :splitter-ns
  \- :splitter-ew
  \/ :mirror-ne-sw
  \\ :mirror-nw-se
})

; (println testfile)

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input) ]
    (reduce-kv
      (fn [acc r line]
       (merge
        acc
          (reduce-kv
            (fn [acc2 c ch] (assoc acc2 [r c] (char-to-kw ch)))
            acc
            (vec line)
        )))
     {}
     lines)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))
(def t1 test-input)
; (pp/pprint t1)


(def start { :dir :west :next-pos [0 0] })

(defn new-pos [state]
  (case (:dir state)
    :north (update state :next-pos #(update % 0 inc))
    :south (update state :next-pos #(update % 0 dec))
    :east (update state :next-pos #(update % 1 dec))
    :west (update state :next-pos #(update % 1 inc))
    ))

(defn mirror [state item]
  (case item
    :mirror-ne-sw ; /
      (case (:dir state)
        :north (new-pos (assoc state :dir :east))
        :east (new-pos (assoc state :dir :north))
        :south (new-pos (assoc state :dir :west))
        :west (new-pos (assoc state :dir :south))
      )
    :mirror-nw-se ; \
      (case (:dir state)
        :north (new-pos (assoc state :dir :west))
        :east (new-pos (assoc state :dir :south))
        :south (new-pos (assoc state :dir :east))
        :west (new-pos (assoc state :dir :north))
      )))

; (mirror { :dir :east :next-pos [1 1] } :mirror-ne-sw)
; (new-pos { :dir :west :next-pos [1 1] })

(defn split-beam [item state]
  (case item
    :splitter-ns ; |
      (case (:dir state)
        :north [(new-pos state)]
        :south [(new-pos state)]
        :east [
               (new-pos (assoc state :dir :north))
               (new-pos (assoc state :dir :south)) ]
        :west [
               (new-pos (assoc state :dir :north))
               (new-pos (assoc state :dir :south)) ]
      )
    :splitter-ew ; -
      (case (:dir state)
        :east [(new-pos state)]
        :west [(new-pos state)]
        :north [
               (new-pos (assoc state :dir :east))
               (new-pos (assoc state :dir :west)) ]
        :south [
               (new-pos (assoc state :dir :east))
               (new-pos (assoc state :dir :west)) ]
      )))

(defn traverse
  ([points] (traverse points start))
  ([points dir] (traverse points #{} (conj [] dir)))
  ([points seen queue]
   (cond
     (empty? queue) seen
     (seen (peek queue)) (recur points seen (pop queue))
     :default (let [state (peek queue)
           pos (:next-pos state)
           next-item (points pos)]
       ; (println (:next-pos state) (:dir state)  next-item (pop queue))
       (case next-item
         nil (recur points seen (pop queue))
         :empty (recur
                  points
                  (conj seen state)
                  (conj (pop queue) (new-pos state))
                 )
         (:splitter-ew :splitter-ns) (recur
                  points
                  (conj seen state)
                  (into (pop queue) (split-beam next-item state))
                  )
         (:mirror-nw-se :mirror-ne-sw) (recur
                  points
                  (conj seen state)
                  (conj (pop queue) (mirror state next-item))
                 ))))))

; (traverse t1 [] [{ :dir :west :next-pos [123 123]}])
; (traverse t1)
; (count (traverse t1))

(defn part1 [input]
  (count (into #{} (map :next-pos (traverse input))) ))

; (pp/pprint (map hash1 input))

; (part1 t1)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)

(defn generate-starts [points]
  (let [mr (reduce max (map first (keys points)))
        mc (reduce max (map second (keys points)))
        ]
    (apply concat [
         (mapv (fn [i] { :dir :north :next-pos [0 i]}) (range 0 (inc mr)))
         (mapv (fn [i] { :dir :south :next-pos [mr i]}) (range mr -1 -1))
         (mapv (fn [i] { :dir :west :next-pos [i 0]}) (range 0 (inc mc)))
         (mapv (fn [i] { :dir :east :next-pos [i mc]}) (range mc -1 -1))
                   ])))

; (pp/pprint (generate-starts t1))
(defn traverse-and-count [input start]
  (count (into #{} (map :next-pos (traverse input start)))))

; (traverse-and-count t1 start)
; (traverse-and-count t1 { :dir :north :next-pos [0 3]})
; (traverse-and-count t1 { :dir :north :next-pos [0 5]})


(defn part2 [input]
  (reduce max
          (map #(traverse-and-count input %)  (generate-starts input))
   ))

; (part2 t1)
(part2 input)
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
