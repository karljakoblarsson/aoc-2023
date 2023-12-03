(ns aoc.day3
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

(def day 3)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))


(defn re-seq-pos [pattern string] 
  (let [m (re-matcher pattern string)] 
    ((fn step [] 
      (when (. m find) 
        (cons {:start (. m start) :end (. m end) :group (. m group)} 
          (lazy-seq (step))))))))


(defn parse-symbol-line [i s]
  (map #(assoc % :pos [i (:start %)] )
    (re-seq-pos #"[^.0123456789]" s)
       ))

(defn parse-symbols [lines]
  (flatten
    (map-indexed (fn [i s]  (parse-symbol-line i s)) lines))
   )


(defn parse-numbers-line [i s]
  (let [matches (re-seq-pos #"\d+" s)]
    (map (fn [m]
           {
            :row i
            :sc (:start m)
            :ec (dec (:end m))
            :num (parse-int (:group m))
            }
           ) matches)
    ))

(defn parse-numbers [lines]
  (flatten (map-indexed parse-numbers-line lines)))

(parse-numbers-line 1   "...$...654..12")
(parse-numbers   [ "...$...654..12"])
(parse-symbols   [ "...$...654..12"])

; (parse-symbol-line "...$...654..")
; (re-seq-pos #"\d+"  "...$...654..12")
; (parse-line ts)

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        symbols (parse-symbols lines)
        numbers (parse-numbers lines)]
    { :symbols symbols
     :numbers numbers
     }
    )
  )

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

; (:symbols t1)
; (:numbers t1)

(defn adjacent [row sc se]
  (let [cols (range (dec sc) (+ 2 se))]
    (apply concat (map
                    (fn [c] [
       [(dec row) c]
       [(inc row) c]
       [row       c]
                           ])
                       cols)))
   ; [(dec row) (dec sc)]
   ; [(inc row) (dec sc)]
   ; [row       (dec sc)]
  )

; (adjacent 3 4 6)

; (some #{[2 5]}
;   (adjacent 3 4 6))

; (:numbers t1)
(defn filter-valid [{:keys [symbols numbers]}]
  (let [symbs (set (map :pos symbols))
        test-fn (fn [{:keys [row sc ec]}]
                  (some symbs (adjacent row sc ec)))
        ]
    (filter test-fn numbers)
    ))

; (adjacent 5 7 8 )
; (pp/pprint (map :pos (:symbols t1)))

; (pp/pprint (filter-valid t1))
; (sum (map :num (filter-valid t1)) )

(defn part1 [input]
  (sum (map :num (filter-valid input)) ))

; (part1 t1)
; (part1 input)
; (println (time (part1 input)))


; (:symbols t1)
(defn filter-star [{ s :symbols}]
  (filter #(= (:group %) "*") s))

; (filter-star t1)

(defn get-adj [{ nmbs :numbers} { p :pos }]
  (filter
    (fn [{:keys [row sc ec]}] (some #{p} (adjacent row sc ec)))
    nmbs))

; (get-adj t1 { :pos [1 3]})

(defn get-gears [in]
  (->> in
      (filter-star)
      (map #(get-adj in %))
      (filter #(= 2 (count %)))
   ))

; (pp/pprint (get-gears t1))
; (pp/pprint (:numbers t1))


(defn part2 [input]
  (->> input
       (get-gears)
      (map #(map :num %))
      (map #(reduce * 1 %))
      (sum)
       ))

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
