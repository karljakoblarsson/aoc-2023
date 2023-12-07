(ns aoc.day7
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

(def day 7)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(defn char-to-rank [c]
  ({
    \A 14
    \K 13
    \Q 12
    \J 11
    \T 10
    \9 9
    \8 8
    \7 7
    \6 6
    \5 5
    \4 4
    \3 3
    \2 2
    } c)
  )

(defn parse-line [s]
  (let [[cards score] (s/split s #" ")]
    { :cards (mapv char-to-rank cards) :score (parse-int score)}
    ))

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input) ]
    (mapv parse-line lines)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)
; [{:cards [3 2 10 3 13], :score 765}
;  {:cards [10 5 5 11 5], :score 684}
;  {:cards [13 13 6 7 7], :score 28}
;  {:cards [13 10 11 11 10], :score 220}
;  {:cards [12 12 12 11 14], :score 483}]

(defn group [cs] (partition-by identity (sort cs)))

(defn has-group-n [n gs]
  (some #{n} (map count gs)))

; (has-group-n 3 (group [ 4 4 8 4 4]))

(defn highest-suit [cs]
  (let [gs (group cs)]
    (cond
      (has-group-n 5 gs) :five-of-a-kind
      (has-group-n 4 gs) :four-of-a-kind
      (and (has-group-n 3 gs) (has-group-n 2 gs))  :full-house
      (has-group-n 3 gs) :three-of-a-kind
      (= [1 2 2] (sort (map count gs))) :two-pair
      (has-group-n 2 gs) :one-pair
      :default :high-card
     )))

(defn cmp-same [as bs]
  (->> (map compare as bs)
  (filter #(not (= 0 %)) )
  (first)
    ))

; (cmp-same [1 1 3 5 4] [1 1 2 4 5])

(def suit-rank {
       :five-of-a-kind 7
       :four-of-a-kind 6
       :full-house 5
       :three-of-a-kind 4
       :two-pair 3
       :one-pair 2
       :high-card 1
    })

(defn cmp-suit [a b]
  (compare (suit-rank a) (suit-rank b)))

(defn cmp-hand [a b]
  (let [as (highest-suit a)
        bs (highest-suit b)
        cs (cmp-suit as bs)
        ]
    (if (= cs 0)
      (cmp-same a b)
      cs
      )
    ))

(defn cmp-play [a b]
  (cmp-hand (:cards a) (:cards b)))

; (sort-by)

(defn part1 [input]
  (let [sorted (sort-by :cards cmp-hand input)
        ]
    (reduce-kv (fn [sum i v] (+ sum (* (inc i) (:score v)))) 0 (vec sorted))
  ))

; (part1 t1)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println t1)

(defn remap-j [p]
  (update p :cards (fn [cs] (map (fn [c] (if (= c 11) 1 c)) cs))))

; (pp/pprint (map remap-j t1))

(defn h-g-n-2 [js n gs]
  (some #{(- n js)} (map count gs))
  )

(frequencies (map count (group [1 1 1 2 5])))

(defn is-full-house [cs]
  (let [js (count (filter #(= 1 %) cs))
        rs (filter #(not (= 1 %)) cs)
        freqs (->> rs
                   (group)
                   (map count)
                   (frequencies))
        has-two-pair (some-> (freqs 2) (= 2))
        has-three (some-> (freqs 3) (= 1))
        has-pair (some-> (freqs 2) (>= 1))
        has-single (some-> (freqs 1) (>= 1))
        ]
    ; (println js groups has-three has-pair has-single)
    (cond
      (and has-three has-pair) :full-house
      (and has-three (>= js 1)) :full-house
      ; has two pair
      (and has-two-pair (= js 1)) :full-house
      (and has-pair has-single (>= js 2)) :full-house
      :default false
      )
  ))

; (is-full-house [1 1 3 3 5])
; (is-full-house [5,4,1,7,1])


; correct for t1 not for input
(defn highest-suit-2 [cs]
  (let [js (count (filter #(= 1 %) cs))
        gs (group (filter #(not (= 1 %)) cs))]
    (cond
      (= js 5) :five-of-a-kind
      (h-g-n-2 js 5 gs) :five-of-a-kind
      (h-g-n-2 js 4 gs) :four-of-a-kind
      (is-full-house cs)  :full-house
      (h-g-n-2 js 3 gs) :three-of-a-kind
      (= [1 2 2] (sort (map count gs))) :two-pair
      (h-g-n-2 js 2 gs) :one-pair
      :default :high-card
     )))

(t/are [i o] (= (highest-suit-2 i) o)
  [1 1 1 1 1] :five-of-a-kind
  [2 1 1 1 1] :five-of-a-kind
  [1 2 3 4 5] :one-pair
  [7 2 3 4 5] :high-card
  [7 7 1 7 5] :four-of-a-kind
  [7 7 3 7 5] :three-of-a-kind
  [7 7 5 7 5] :full-house
  [7 1 5 7 5] :full-house
  [7 7 1 7 5] :four-of-a-kind
  [7 7 1 7 1] :five-of-a-kind
  [5 7 1 7 1] :four-of-a-kind
  [5 4 1 7 1] :three-of-a-kind
  [5 5 1 7 1] :four-of-a-kind
       )

(defn cmp-hand-2 [a b]
  (let [as (highest-suit-2 a)
        bs (highest-suit-2 b)
        cs (cmp-suit as bs)
        ]
    (if (= cs 0)
      (cmp-same a b)
      cs
      )
    ))

; (pp/pprint (sort-by :cards cmp-hand-2 (map remap-j t1)))

(defn part2 [input]
  (let [in' (map remap-j input)
        sorted (sort-by :cards cmp-hand-2 in')
        ]
    (reduce-kv (fn [sum i v] (+ sum (* (inc i) (:score v)))) 0 (vec sorted))
  ))

; correct for t1 not for input

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
