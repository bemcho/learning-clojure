(ns learning-clojure.core
  (:gen-class))
(import 'java.io.File)
(require '[clojure.set :as s])
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
  (set (for [m compositions] (apply merge (for [[key val] m] {(get key-map key key) val})))))

(defn select
  "Selects entries from relation approved by predicate eg:
  (select #(= (:name %) \"Requiem\") compositions)"
  [pred relation]
  (set (for [m relation :when (pred m)] m)))

(defn project
  "Selects that specifies a subset of columns in keys eg:
  (project compositions [:name])"
  [relation keys]
  (set (for [m relation] (apply merge (for [[key val] m :when ((set keys) key)] {key val})))))

(defn join
  "Joins two relations on shared key eg:
  (join compositions composers)"
  [relation-1 relation-2]
  (set (for [m1 relation-1 m2 relation-2
             :let [common-key (first (s/intersection (set (keys m1)) (set (keys m2))))]
             :when (= (m1 common-key) (m2 common-key))]
         (merge-with #(identity %2) m1 m2))))

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