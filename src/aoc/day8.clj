(ns aoc.day8
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

(def day 8)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))
(def testfile2 (slurp "inputs/test8b.txt"))
(def testfile3 (slurp "inputs/test8c.txt"))


(defn to-kw [s] (keyword s))

(defn parse-line [s]
  (let [[s from left right] (re-matches #"^(.{3}) = \((.{3}), (.{3})\)$" s)]
    [(to-kw from) { :L (to-kw left) :R (to-kw right)}  ]
    ))

; (parse-line "AAA = (BBB, CCC)")
; (re-matches #"^([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)$" "AAA = (BBB, CCC)")

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
       gs (partition-by-empty-line lines) ]
    { :inst (map #(keyword (str %)) (first (first gs)))
     :paths (into {} (map parse-line (second gs))) 
     }
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
(def t2 (prepare-input testfile2))
(def t3 (prepare-input testfile3))

; (pp/pprint t3)


; (reductions (fn [prev n] (((:paths t1) prev) n)  ) :A (:inst t1))
(defn steps [{ :keys [paths inst]}]
  (reductions
    (fn [prev n] ((paths prev) n)) :AAA (cycle inst))
  )

(def end :ZZZ)

(defn step-until [df]
  (take-while
   #(not (= % end))
   (steps df)
   ))

; (step-until t2)

(defn part1 [input]
  (let []
    (count (step-until input))
  ))

; (part1 t1)
; (part1 t2)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println t1)


(defn is-start [kw]
  (= \A (last (str kw))))

(defn is-end [kw]
  (= \Z (last (str kw))))

(defn starts [{:keys [paths]} ]
  (filter is-start (keys paths)))
; (starts t3)

(defn steps2 [{ :keys [paths inst] :as all}]
  (reductions
    (fn [prev n]  (map #(% n) (map paths prev)) ) (starts all) (cycle inst))
  )


(defn step-until2 [df]
  (take-while
   #(not (every? is-end %))
   (steps2 df)
   ))

(defn next-inst [insts i]
  (mod (inc i) (count insts))
  )

; (next-inst [1 3 3 3] 4)

(defn step-until2-loop [{ :keys [paths inst] :as all}]
  (let [inst-len (count inst)
        insts (vec inst)
        next-inst-c (fn [i] (mod (inc i) inst-len))]
    (loop [st (starts all)
           c 0
           inst-c 0
           ]
        ; (println c st (nth insts inst-c))
      (if (every? is-end st)
        c
        (recur
          (mapv (nth insts inst-c) (mapv paths st))
          (inc c)
          (next-inst-c inst-c))
        )
      )))

; (pp/pprint (starts t3)) ; (:11A :22A)

; (pp/pprint (starts input))
; '(:AAA :VDA :VSA :GTA :BBA :GPA)


(defn find-loop [{ :keys [paths inst] :as all} st]
  (let [inst-len (count inst)
        insts (vec inst)
        next-inst-c (fn [i] (mod (inc i) inst-len))]
    (loop [curr st
           c 0
           inst-c 0
           seen #{}
           path []
           ]
        ; (println c st (nth insts inst-c))
      (let [next-in (nth insts inst-c)]
        (if (seen [curr next-in inst-c])
          { :c c
            :path path
            :loop-point [curr next-in]
           }
          (recur
            (next-in (paths curr))
            (inc c)
            (next-inst-c inst-c)
            (conj seen [curr next-in inst-c])
            (conj path [curr next-in])
           )
          )
        )
      )))

; (pp/pprint (find-loop t3 :11A))
; (pp/pprint (find-loop t3 :22A))
; (pp/pprint (find-loop input :AAA))

(defn find-points [{ :keys [c path loop-point]}]
  (let [ls (.indexOf path loop-point)
        le c
        zi (remove nil?
                   (map-indexed
                     (fn [i v] (if (is-end (first v)) i nil))
                     path))]
    { :loop-start ls
      :loop-end le
      :z-index zi
     }
    ))


(defn build-i-seq
 ([obj]
   (build-i-seq obj 0)
   )
  ([{ :keys [loop-start loop-end z-index] :as all} n]
     (lazy-seq (concat
                 (map
                   #(+ % (* n (- loop-end loop-start)))
                   z-index)
                 (build-i-seq all (inc n))))
   )
  )

(defn find-lowest [seqs]
  (if (apply = (map first seqs))
             (first (first seqs))
             (let [[_ li] (reduce-kv
                            (fn [[n li] i s]
                              (if (< (first s) n)
                                [(first s) i]
                                [n li]))
                           [##Inf 0] 
                           seqs)]
               (recur (update seqs li rest))
               )))

; (find-lowest [(map #(* 2 %) (rest (range))) (map #(* 3 %) (rest (range)))])

; (pp/pprint (find-loop t3 :11A) )
; (pp/pprint (find-points (find-loop t3 :11A)))
; (->> :11A
;      (find-loop t3)
;      (find-points)
;      (build-i-seq)
;      (take 10)
;      )
; (pp/pprint (find-loop t3 :22A) )
; (->> :22A
;      (find-loop t3)
;      (find-points)
;      (build-i-seq)
;      (take 10)
;      )

; (->> :GPA
;      (find-loop input)
;      (find-points)
;      (build-i-seq)
;      (take 10)
;      )

; (13771 27542 41313 55084 68855 82626 96397 110168 123939 137710)
; (+ 13771 (* 1 13771)) ; 27540

(defn part2 [input]
  (let [ss (vec (starts input))
        ls (mapv #(find-loop input %) ss)
        ps (mapv find-points ls)
        seqs (mapv build-i-seq ps)
        ]
    ; (pp/pprint ss)
    ; (pp/pprint ls)
    (pp/pprint ps)
    (find-lowest seqs)
    ))

; (while)

; (part2 t3)
; (part2 input)
(println (time (part2 input)))


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
