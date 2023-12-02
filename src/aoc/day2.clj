(ns aoc.day2
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

(def day 2)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

(def ts "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
; (println
;  (re-matches #"Game (\d+):( (\d+) ((red|green|blue))[;$])+" ts)
;  )
; (println ts)
; (pp/pprint
;  (re-matches #"Game (\d+):(( (\d+) (red|green|blue),?)+;?)+$" ts)
;  )

(defn int-if [s] (if (nil? s) nil (parse-int s)))

(defn parse-draw [s]
  (let [
        [_ r _] (re-matches #".*?(\d+) red.*?"  s)
        [_ g _] (re-matches #".*?(\d+) green.*?"  s)
        [_ b _] (re-matches #".*?(\d+) blue.*?"  s)
        ]
    {
     :red (int-if r)
     :green (int-if g)
     :blue (int-if b)
    }
    ))

; (pp/pprint (re-matches #".*(\d+) red.*" " 3 red, 5 blue"))

; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
(defn parse-line [s]
  (let [[_ game rs] (re-matches #"Game (\d+):(.*)$" s)
        draws (s/split rs #";")
        ]
    { :game (parse-int game) :draws (mapv parse-draw draws) }
    ))

; (parse-line ts)

(defn prepare-input [str-input]
  (map parse-line (s/split-lines str-input)))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (println t1)

 ; 12 red cubes, 13 green cubes, and 14 blue cubes

(defn is-over [n m] (if (nil? n) nil (> n m)))

(defn draw-is-invalid [{ :keys [red green blue]}]
  (or
   (is-over red 12)
   (is-over green 13)
   (is-over blue 14)
   ))

(draw-is-invalid { :red 8 :green nil :blue 1})

(is-over nil 12)
(def t2 [:game 1 :draws [
                         { :red 0 :green 8 :blue 6}
                         { :red 4 :green 8 :blue 6}
                         { :red 1 :green 8 :blue 6}
                         ]])

(defn is-valid [{ :keys [draws]}]
  (not-any? draw-is-invalid draws)
  )

(defn filter-valid [gs]
  (filter is-valid gs)
  )

(pp/pprint (filter-valid t1))

(defn calc-id-sum [gs]
  (sum (map :game gs)))

(defn part1 [input]
  (calc-id-sum (filter-valid input)))

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))

(defn max-if [a b]
  (cond
    (nil? b) a
    (nil? a) b
    true (max a b)
    ))

(defn min-draw [ds]
  (reduce #(merge-with max-if %1 %2) {} ds))

(min-draw [{:r 1} {:b 2 :r 4} {:r nil}])

(defn min-cubes [{ :keys [draws]}]
  (min-draw draws)
  )

; (pp/pprint t1)
; (min-cubes (first t1))
; (println (:draws (first t1)))

(map min-cubes t1)

(defn cube-power [{ :keys [red green blue]}]
  (* red green blue))


(defn part2 [input]
  (->> input
      (map min-cubes)
      (map cube-power)
      (sum)
       ))

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
