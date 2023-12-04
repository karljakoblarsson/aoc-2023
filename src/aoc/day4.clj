(ns aoc.day4
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

(def day 4)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (defn parse-array [s]
;   (read-string (str "[ " s " ]")))

(defn parse-line [l]
  (let [[win card] (s/split l #"\|")
        [_ game numbs] (re-matches #"^Card +(\d+):(.*)" win)
        ]
    { :wins (parse-array numbs)
      :plays (parse-array card)
      :game (parse-int game)
    }
    ))

; (parse-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn prepare-input [str-input]
  (let [lines (s/split-lines str-input)
        plays (map parse-line lines) ]
    plays
    ))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)
; (pp/pprint t1)

(defn filter-wins [{ :keys [wins plays ]}]
  (let [ws (set wins)]
    (filter ws plays))
  )

; (play-to-score (filter-wins (first t1)))

(defn expt [b n] (reduce * (repeat n b)))
; (expt 2 0)
(defn score [n]
  (if (= 0 n) 0
      (expt 2 (dec n))
      ))

(defn play-to-score [l]
  (score (count l)))

; (map (fn [l] (play-to-score (filter-wins l))) t1 )

(defn part1 [input]
  (sum
    (map (fn [l]
         (play-to-score (filter-wins l))
         )
       input )))

; (part1 t1)
; (pp/pprint input)
; (part1 input)
; (println (time (part1 input)))

(defn get-copies [{ :keys [game] :as play}]
  (let [wins (count (filter-wins play))]
    { :game game :copies (vec (range (inc game) (+ game wins 1)))}
    ))

; (map (comp vals get-copies ) t1)

(defn build-lookup [plays]
  (into (sorted-map) (mapv (comp vec vals get-copies) plays)))

; (build-lookup t1)

; (defn copies [lookup card]
;   (let [cs (lookup card)]
;     (if (= nil cs)
;       [card]
;       (conj [card] (map #(copies lookup %) cs )  ))
;     ))

; (flatten (copies (build-lookup t1) 4))

; (defn run-game [plays]
;   (let [wins (filter
;               #(> % 0)
;               (map (comp play-to-score filter-wins) plays))
;         lookup (build-lookup plays)
;         ]
;     (map #(copies lookup %) wins)
;     ))

(defn step [lookup cards]
  (remove nil?  (apply concat (map lookup cards))))
; (step (build-lookup t1) [1])

(defn run-game [plays]
  (let [wins (filter
              #(> % 0)
              (map (comp play-to-score filter-wins) plays))
        lookup (build-lookup plays)
        ]
        ; (take-while #(not (empty? %)) (iterate #(step lookup %) wins))
        (take 13  (iterate #(step lookup %) wins))
    ))
; (run-game input)
; (println (part2 input))

; (->> [1]
;   (step (build-lookup input))
;   (step (build-lookup input))
;      )

; ((build-lookup t1) 1)
; (build-lookup t1) 
; (reduce-kv (fn [acc k v] (assoc acc k (count v))) {} (build-lookup t1) )

; { 1 1  3 1}
; { 2 1  3 1  4 2  5 2 }

(defn step-map [lookup cards]
  (reduce-kv
    ; (fn [acc c] (assoc acc c (count (lookup c))))
    (fn [acc k v]
     (let []
        (assoc acc c (count (lookup c)))
       )
     )
   {}
   cards))
; (step-map (build-lookup t1) { 1 1 })

(defn run-game-2 [plays]
  (let [
        wins (filter #(> % 0)
              (map (comp play-to-score filter-wins) plays))
        lookup (build-lookup plays)
        ]
    ))

(def tl (build-lookup input))

(def no-cards
  (memoize
    (fn [card]
     (println card)
     (let [nx (tl card)
           to-add (inc (count nx)) ]
      (reduce + 1 (map no-cards (tl card)))
                         ))))


; (count (flatten (run-game t1)))

(defn part2 [plays]
  (let [wins (filter #(> % 0)
              (map (comp play-to-score filter-wins) plays))
        ]
    (sum (map no-cards (map :game plays)))
    )
  )

; (count t1)
; (part2 t1)

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
