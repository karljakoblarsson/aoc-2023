(ns aoc.day13
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   [clojure.math.numeric-tower :as ma]
   )
  (:use aoc.core))


(def day 13)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        images (partition-by-empty-line lines)
       ]
    (map (fn [img] (mapv vec img)) images)
    ))

; (pp/pprint (prepare-input testfile))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn transpose [m]
    (apply mapv vector m))

; (transpose [[1 2] [3 4]])

(defn is-mirror [[a b]]
  (every? #(= (first %) (second %)) (map vector (reverse a) b)))

; (map vector [1 2] [5 6 7])

(def t2 (first t1))

(pp/pprint t2)

; (pp/pprint (split-at 4 (transpose t2)))
; (is-mirror (split-at 5 (transpose t2)) )

; (split-at 8 [1 2 3 4 5 6 7 8])
(defn find-mirror-index [img]
  (let [max-row (count img)]
    (first
      (filter
        #(is-mirror (split-at % img))
        (range 1 max-row)
            ))))

; (find-mirror-index t2)
(defn find-any-mirror [img]
  (let [column (find-mirror-index (transpose img))
        row (find-mirror-index img)
        ]
    { :col column
      :row row
     }))

(defn part1 [input]
  (let [mirrors (map find-any-mirror input)
        rs (remove nil? (map :row mirrors))
        cs (remove nil? (map :col mirrors))
        ]
    (+ (sum cs) (* 100 (sum rs)))
    )
  )

; (part1 t1)
; (part1 input)
; (pp/pprint (part1 t1))
; (part1 input)
; (println (time (part1 input)))

; (println input)

(def change { \# \. \. \# })

(defn find-any-mirror2 [img]
  (let [column (find-mirror-index (transpose img))
        row (find-mirror-index img)
        ]
    (remove
      nil?
      [
      (if column { :type :col :index column } nil)
      (if row { :type :row :index row } nil)
      ])
    ))

(defn change-pos [img [r c]]
  (update img r (fn [v] (update v c change))))

(defn iterate-pos [mr mc]
  (apply concat
    (map #(map (fn [c] [% c]) (range mc)) (range mr)) ))

; (iterate-pos 3 3)

; (count (first t2))
; (println (iterate-pos 7 9))
; (count [1 2 34])
; (assoc [1 2 34] 3 12)


(defn find-smudge [img]
  (let [max-row (count img)
        max-col (count (first img))
        old-line (find-any-mirror2 img)]
    (first
      (filter
        (fn [i] (let [l (find-any-mirror2 i)]
                (if (nil? l)
                  false
                  (not (= l old-line)))
                ))
        (map #(change-pos img %) (iterate-pos max-row max-col))))
    )
  )

(defn find-new-line [img]
  (let [max-row (count img)
        max-col (count (first img))
        old-line (find-any-mirror2 img)]
    (some identity
      (map
        (fn [i] (let [l (find-any-mirror2 i)]
                (if (nil? l)
                  nil
                  (if (not (= l old-line)) l nil))
                ))
        (map #(change-pos img %) (iterate-pos max-row max-col))))
    )
  )

(pp/pprint (find-new-line (first t1)))
; (pp/pprint (find-any-mirror2 (find-smudge (second t1))) )


(defn part2 [input]
  (let [new-imgs (map find-smudge input)
        old-lines (map find-any-mirror2 input)
        mirrors (map find-any-mirror2 new-imgs)
        diff (map (fn [n o] (vec (st/difference (set n) (set o)))) mirrors old-lines)
        diff' (apply concat diff)
        rs (filter #(= (:type %) :row) diff')
        cs (filter #(= (:type %) :col) diff')
        rsc (remove nil? (map :index rs))
        csc (remove nil? (map :index cs))
        ]
    ; (println old-lines)
    ; (println mirrors)
    ; (println rs cs rsc csc)
    ; (pp/pprint rs)
    ; (pp/pprint new-imgs)
    ; (pp/pprint old-lines)
    (pp/pprint mirrors)
    (+ (sum csc) (* 100 (sum rsc)))
    ))

; (part2 t1)
(part2 input)
; 5546 is to low
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
