(ns learning-clojure.core
  (:import (java.io File))
  (:require [clojure.set :as set])
  (:gen-class))

(def visitors (atom #{}))

(defn hello-with-memory [name]
  (if (contains? @visitors name)
    (str "Hello, " name ". Welcome back!")
    (do (swap! visitors conj name)
        (str "Hello, " name))))

(defn indexed
  "Returns vector of vectors [idx char]"
  [coll]
  (map-indexed vector coll))

(defn index-filter
  "docstring"
  [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn index-of-any
  "docstring"
  [pred coll]
  (first (index-filter pred coll)))

(def primes
  (concat
    [2 3 5 7]
    (lazy-seq
      (let [primes-from
            (fn primes-from [n [f & r]]
              (if (some #(zero? (rem n %))
                        (take-while #(<= (* % %) n) primes))
                (recur (+ n f) r)
                (lazy-seq (cons n (primes-from (+ n f) r)))))
            wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2
                          6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6
                          2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
        (primes-from 11 wheel)))))
(def ordinals-and-primes (map vector (iterate inc 1) primes))

(defn minutes-to-millis [mins] (* mins 1000 60))
(defn recently-modified? [file]
  (> (.lastModified file)
     (- (System/currentTimeMillis) (minutes-to-millis 30))))

(defn rename
  "Renames key in key-map eg:
  (rename compositions {:name :title})"
  [in-map key-map]
  (set (for [m in-map] (apply merge (for [[key val] m] {(get key-map key key) val})))))
;(rename compositions {:name :title})
;=>
; #{{:title "Requiem", :composer "Giuseppe Verdi"}
;     {:title "Musical Offering", :composer "J.S. Bach"}
;     {:title "Requiem", :composer "W. A. Mozart"}
;     {:title "The Art of the Fugue", :composer "J.S. Bach"}}

(defn select
  "Selects entries from relation approved by predicate eg:
  (select #(= (:name %) \"Requiem\") compositions)"
  [pred relation]
  (set (for [m relation :when (pred m)] m)))
;(select #(= (:name %) "Requiem") compositions)
;=>
; #{{:name "Requiem", :composer "W. A. Mozart"}
;     {:name "Requiem", :composer "Giuseppe Verdi"}}

(defn project
  "Selects that specifies a subset of columns in keys eg:
  (project compositions [:name])"
  [relation keys]
  (set (for [m relation] (apply merge (for [[key val] m :when ((set keys) key)] {key val})))))
;(project
;  (join
;    (select #(= (:name %) "Requiem") compositions) composers)
;  [:country])
;=>
; #{{:country "Italy"} {:country "Austria"}}

(defn join
  "Joins two relations on shared key eg:
  (join compositions composers)"
  [relation-1 relation-2 & [[src dst] :as mapping]]
  (set (for [m1 relation-1 m2 relation-2
             :let [common-key (first (set/intersection (set (keys m1)) (set (keys m2))))]
             :when (cond mapping (= (m1 src) (m2 dst))
                         true (= (m1 common-key) (m2 common-key)))]
         (merge-with #(identity %2) m1 m2))))
;(join composers nations [:country :nation])
;=>
;#{{:composer "W. A. Mozart", :country "Austria", :nation "Austria", :language "German"}
;  {:composer "J. S. Bach", :country "Germany", :nation "Germany", :language "German"}
;  {:composer "Giuseppe Verdi", :country "Italy", :nation "Italy", :language "Italian"}}

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Starting -main"))

(def compositions
  #{{:name "The Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering" :composer "J. S. Bach"}
    {:name "Requiem" :composer "Giuseppe Verdi"}
    {:name "Requiem" :composer "W. A. Mozart"}})
(def composers
  #{{:composer "J. S. Bach" :country "Germany"}
    {:composer "W. A. Mozart" :country "Austria"}
    {:composer "Giuseppe Verdi" :country "Italy"}})
(def nations
  #{{:nation "Germany" :language "German"}
    {:nation "Austria" :language "German"}
    {:nation "Italy" :language "Italian"}})

;;;Lazy sequences
(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

;;;;;;;;;;;;;;;;;;;
(def ^{:doc "Count items matching a filter"}
count-if (comp count filter))

(defn count-runs
  "Count runs of length n where pred is true in coll."
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))
;;;;;;;;;;;;;;;;;;;
(defn parity [n]
  (loop [n n par 0]
    (if (= n 0)
      par
      (recur (dec n) (- 1 par)))))

;;;;;;;;;;;;;;;;;;
(defn deeply-nested [n]
  (loop [n n
         result '(bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- coll-or-scalar [x & _] (if (coll? x) :collection :scalar))
(defmulti replace-symbol coll-or-scalar)

(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq
    (when (seq coll)
      (cons (replace-symbol (first coll) oldsym newsym)
            (replace-symbol (rest coll) oldsym newsym)))))

(defmethod replace-symbol :scalar [obj oldsym newsym]
  (if (= obj oldsym) newsym obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memoization
(declare m f)
(defn m [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))

(defn f [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))
(def m (memoize m))
(def f (memoize f))

(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Transducers
(defn preds-seq []
  (->> (all-ns)
       (map ns-publics)
       (mapcat vals)
       (filter #(clojure.string/ends-with? % "?"))
       (map #(str (.-sym %)))
       vec))

(defn preds []
  (into []
        (comp (map ns-publics)
              (mapcat vals)
              (filter #(clojure.string/ends-with? % "?"))
              (map #(str (.-sym %))))
        (all-ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Specs
