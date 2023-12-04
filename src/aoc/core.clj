(ns aoc.core)

(defn mk-input-filename [n]
  (str "inputs/day" n ".txt"))

(defn mk-test-input-filename [n]
  (str "inputs/test" n ".txt"))

(defn ifv [pred v f]
  (if (pred v) (f v) v))

(defn call-if [bool f v]
  (if bool (f v) v))

(defn count-pred [pred coll]
  (reduce (fn [acc e] (call-if (pred e) inc acc)) 0 coll))

(defn sum [coll] (reduce +' coll))

; (defn common-elements [lst] (apply st/intersection (map set lst)))

(defn partition-by-empty-line [lst]
  (remove #(every? empty? %) (partition-by empty? lst))
  )

(defn parse-int [s] (Long/parseLong s))

(defn parse-double [s] (Double/parseDouble s))
(def parse-float parse-double)

(defn parse-array [s]
  (read-string (str "[ " s " ]")))

; (defn re-seq-pos [pattern string]
;   (let [m (re-matcher pattern string)]
;     ((fn step []
;       (when (. m find)
;         (cons {:start (. m start) :end (. m end) :group (. m group)}
;           (lazy-seq (step))))))))


