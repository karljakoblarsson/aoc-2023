(ns aoc.day10
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

(def day 10)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))
(def testfile2 (slurp "inputs/test10b.txt"))

(def testfile3 (slurp "inputs/test10c.txt"))
(def testfile4 (slurp "inputs/test10d.txt"))
(def testfile5 (slurp "inputs/test10e.txt"))



(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       ]
    lines
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))


    ; | is a vertical pipe connecting north and south.
    ; - is a horizontal pipe connecting east and west.
    ; L is a 90-degree bend connecting north and east.
    ; J is a 90-degree bend connecting north and west.
    ; 7 is a 90-degree bend connecting south and west.
    ; F is a 90-degree bend connecting south and east.
    ; . is ground; there is no pipe in this tile.
    ; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

(def char-to-kw
  {
    \| :ns
    \- :ew
    \L :ne
    \J :nw
    \7 :sw
    \F :se
    \. :ground
    \S :start
   })


(defn to-map [lines]
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
  )

; (to-map test-input)

(def t1 (to-map test-input))
(def t2 (to-map (prepare-input testfile2)))
; (pp/pprint t2)

(def t3 (to-map (prepare-input testfile3)))
(def t4 (to-map (prepare-input testfile4)))
(def t5 (to-map (prepare-input testfile5)))

(defn step [mp from curr]
  (let [dir (mp curr)
        fr (first from)
        fc (second from)
        cr (first curr)
        cc (second curr)
        ns-diff (- cr fr)
        ew-diff (- cc fc)
        ]
    ; (println from curr dir ns-diff ew-diff)
    (case dir
      :ns (cond
            (= 1 ns-diff) [(inc cr) cc]
            (= -1 ns-diff) [(dec cr) cc]
            :default nil)
      :ew (cond
            (= 1 ew-diff) [cr (inc cc)]
            (= -1 ew-diff) [cr (dec cc)]
            :default nil)
      :ne (cond
            (= 1 ns-diff) [cr (inc cc)]
            (= -1 ew-diff) [(dec cr) cc]
            :default nil)
      :nw (cond
            (= 1 ns-diff) [cr (dec cc)]
            (= 1 ew-diff) [(dec cr) cc]
            :default nil)
      :sw (cond
            (= -1 ns-diff) [cr (dec cc)]
            (= 1 ew-diff) [(inc cr) cc]
            :default nil)
      :se (cond
            (= -1 ns-diff) [cr (inc cc)]
            (= -1 ew-diff) [(inc cr) cc]
            :default nil)
      :ground nil
      :start :start
      nil nil
      )
    ))

; (pp/pprint t1)
; (step t1 [3 1] [3 2])
; (step t1 [3 2] [3 3])
; (step t1 [0 4] [0 3])
; (step t1 [0 3] [1 3])

; (conj '(1 3) 2)

(defn run-steps [mp start from]
  (take-while
    #(not (or (nil? (first %)) (= :start (first %))))
   (iterate
     (fn [s]
       (let [c (first s)
             f (second s)
             n (step mp f c)
            ]
         ; (println "rs " c f n)
         (conj s n)
        )
      )
     (seq [start from]))
   )
  )

; (conj (seq [1 2]) 5)
; (count (last (run-steps t1 [2 1] [1 1])))

(defn is-loop [start path]
  (= start (first path)))

(defn part1 [input]
  (let [mp (to-map input)
        start-pos (reduce-kv
                    (fn [_ k v] (if (= :start v) (reduced k) nil))
                    nil
                    mp)
        n (update start-pos 0 dec)
        s (update start-pos 0 inc)
        e (update start-pos 1 dec)
        w (update start-pos 1 inc)
        paths (map #(last (run-steps mp % start-pos))  [n s e w])
        lop (first (filter #(is-loop start-pos %) paths)) 
        ]
    ; (println [n s e w])
    ; (println start-pos)
    ; (pp/pprint paths)
    (quot (dec (count lop)) 2)
  ))

; (pp/pprint (part1 (prepare-input testfile)))
; (pp/pprint (part1 (prepare-input testfile2)))
; (part1 test-input)
; (part1 t2)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))
; (println (time (part1 (prepare-input testfile3))))
; (println (time (part1 (prepare-input testfile4))))
; (println (time (part1 (prepare-input testfile5))))

; (println input)

(defn get-path [mp]
  (let [start-pos (reduce-kv
                    (fn [_ k v] (if (= :start v) (reduced k) nil))
                    nil
                    mp)
        n (update start-pos 0 dec)
        s (update start-pos 0 inc)
        e (update start-pos 1 dec)
        w (update start-pos 1 inc)
        paths (map #(last (run-steps mp % start-pos))  [n s e w])
        lop (first (filter #(is-loop start-pos %) paths))
        ]
    lop
  ))

; (defn walk-path [path side]
;   (let [lookup (set path)]
;     (reduce

;      #{}
;      path
;      (rest path)
;      )
;     ))

; (pp/pprint (get-path t3))

(defn get-start-piece [path]
  (let [start-pos (first path)
        f (second path)
        l (last path)
        [ns-diff ew-diff] (mapv - )])
  )

(defn points [[r c] max-c]
  (map #(vector r %)  (range c (inc max-c))))

; (points [3 5] 10)

(defn is-vertical [path p]
  (#{ :ns :ne :nw } (path p)))

(defn count-ray [mp path-set max-c p]
  (if (path-set p)
    []
    (keep #(and (path-set %) (is-vertical mp %)) (points p max-c))
    ))

; (count-ray (set (get-path t3)) 10 [6 2])
(count-ray t3 (set (get-path t3)) 10 [1 0])
(count-ray t3 (set (get-path t3)) 10 [5 0])
(count-ray t3 (set (get-path t3)) 10 [3 2])
(count-ray t3 (set (get-path t3)) 10 [3 3])
(count-ray t3 (set (get-path t3)) 10 [8 8])
(count-ray (set (get-path t3)) 10 [1 0])
; (pp/pprint (sort (part2 t3)))

(defn get-max-c [mp]
  (reduce max (map second (keys mp))))
(defn get-max-r [mp]
  (reduce max (map first (keys mp))))

; (get-max-c t3) ; 10
; (count-ray (set (get-path t3)) 10 10 [1 1])
(defn is-in [mp path-set max-c p]
  (let [crossing (count (count-ray mp path-set max-c p)) ]
    (cond
      (= 0 crossing) false
      (= 1 (mod crossing 2)) true
      :default false
      )))

; (partition-by nil? [12 1 2 nil 2 3 nil nil 1])
; (is-in (set (get-path t3)) 10 [1 7])

(defn part2 [mp]
  (let [max-r (get-max-r mp)
        max-c (get-max-c mp)
        path-set (set (get-path mp))
        inside (filter #(is-in mp path-set max-c %) (keys mp))
        ]
    inside
  ))

(pp/pprint (sort (part2 t3)))
(pp/pprint (count (part2 t3)))
(pp/pprint (count (part2 t4)))
(pp/pprint (count (part2 t5)))
(pp/pprint (count (part2 (to-map input))))


; (while)

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
