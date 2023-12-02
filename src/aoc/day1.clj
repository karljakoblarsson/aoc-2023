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
  (s/split-lines str-input
   ))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

(defn maybe-char [c]
  (let [m (re-matches #"\d" (str c))]
    (println m)
  (if (m)
    (parse-int (str c))
    nil))
    )

(maybe-char \1)
(defn maybe-char1 [c]
  (if (nil?  (re-matches #"\d" (str c)))
    nil
    (parse-int (str c))
   ))


(defn getNums [s] (remove nil?  (map maybe-char1 s)))

(defn line [s]
  (let [nums (getNums s)
        f (first nums)
        l (last nums)]
    (parse-int (s/join [(str f) (str l)]))))


(defn part1 [input]
  (let [res (map line input)] (sum res)))

(part1 input)
; (println (time (part1 input)))



(defn m1 [s] (rest (re-matches   #"(^one)(.*)" s )))
(defn m2 [s] (rest (re-matches   #"(^two)(.*)" s )))
(defn m3 [s] (rest (re-matches   #"(^three)(.*)" s )))
(defn m4 [s] (rest (re-matches   #"(^four)(.*)" s )))
(defn m5 [s] (rest (re-matches   #"(^five)(.*)" s )))
(defn m6 [s] (rest (re-matches   #"(^six)(.*)" s )))
(defn m7 [s] (rest (re-matches   #"(^seven)(.*)" s )))
(defn m8 [s] (rest (re-matches   #"(^eight)(.*)" s )))
(defn m9 [s] (rest (re-matches   #"(^nine)(.*)" s )))
(defn mN [s] (rest (re-matches #"(\d)(.*)" s)))

(defn mmm [s]
  (first
  (filter #(not (empty? %)) 
        [
         (m1 s)
         (m2 s)
         (m3 s)
         (m4 s)
         (m5 s)
         (m6 s)
         (m7 s)
         (m8 s)
         (m9 s)
         (mN s)
         ])
   )
  )

; (mN "2one3asdf23")
; (m1 "2one3asdf23")
; (mmm "sone3asdf23")

(def lookup {
"one" 1
"two" 2
"three" 3
"four" 4
"five" 5
"six" 6
"seven" 7
"eight" 8
"nine" 9
              1 1
              2 2
              3 3
              4 4
              5 5
              6 6
              7 7
              8 8
              9 9
             })

; (defn parse-words [s acc]
;   (if (empty? s)
;     acc
;     (let [m (mmm s)] 
;       (if m
;         (recur (second m) (conj acc (lookup (first m))))
;         (recur (str (rest s)) acc) )
;       )))

; (parse-words "onesd2ds" [])

(defn r1 [s] (s/replace s #"(one)" "one1one"))
(defn r2 [s] (s/replace s #"(two)" "two2two"))
(defn r3 [s] (s/replace s #"(three)" "three3three"))
(defn r4 [s] (s/replace s #"(four)" "four4four"))
(defn r5 [s] (s/replace s #"(five)" "five5five"))
(defn r6 [s] (s/replace s #"(six)" "six6six"))
(defn r7 [s] (s/replace s #"(seven)" "seven7seven"))
(defn r8 [s] (s/replace s #"(eight)" "eight8eight"))
(defn r9 [s] (s/replace s #"(nine)" "nine9nine"))

(defn rep [s] 
  (-> s
      (r1)
      (r2)
      (r3)
      (r4)
      (r5)
      (r6)
      (r7)
      (r8)
      (r9)
      ))

(part1 (map rep t2))
(map println
(map rep t2)
 )
(part1 (map rep input))

(def t2 
  [
"   two1nine"
"   eightwothree"
"   abcone2threexyz"
"   xtwone3four"
"   4nineeightseven2"
"   zoneight234"
"   7pqrstsixteen"
   ])

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
