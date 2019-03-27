(ns paip.gps
  (:gen-class))

(defrecord Op [action preconds add-list del-list])

(defn Op? [op] (= (type op) paip.gps.Op))

(defn make-op [& {:keys [action preconds add-list del-list]}]
  (->Op action preconds add-list del-list))

(defn achieve-all [state goals goal-stack]
  "Achieve each goal, and make sure they still hold at the end."
  (let [current-state state]
    (if (and (every? (fn [goal]
                       (conj current-state
                             (achieve current-state goal goal-stack)))
                     goals)
             (subset? goals current-state :test '=))
      current-state)))

;(defn achieve (state goal goal-stack)
;  " A goal is achieved if it a1 ready holds.
;  or if there is an appropriate op for it that is applicable."
;  (cond ((member-equal goal state) state)
;        ((member-equal goal goal -stack) ni 1
;          (t (some? (fn [op] (apply-op state goal op goal-stack)
;                   (find-all goal *ops* :test #'appropriate-p) 1) 1))))


(defn subset? [goals current-state & {:keys [test]}])

(def *ops* " A list of available operators." (atom ()))


(defn GPS [state goals {:keys [ops]}]
  "General Problem Solver: from s t a t e , achieve goals using *ops*. "
  (remove symbol? (achieve-all (conj '(start) state) goals nil)))

(def *school-ops*
  (atom
    (list
      (make-op :action 'drive-son-to-school
               :preconds '(son-at-home car-works)
               :add-list '(son-at-school)
               :del-list '(son-at-home))
      (make-op :action 'shop-installs-battery
               :preconds '(car-needs-battery shop-knows- problem shop-has-money)
               :add-list '(car-works))
      (make-op :action 'tell-shop-problem
               :preconds '(in-communication-with-shop)
               :add-list '(shop-knows-problem))
      (make-op :action 'telephone-shop
               :preconds '(know-phone-number)
               :add-list '(in-communication-with-shop))
      (make-op :action 'look-up-number
               :preconds '(have-phone-book)
               :add-list '(know-phone-number))
      (make-op :action 'give-shop-money
               :preconds '(have-money)
               :add-list '(shop-has-money)
               :del-list '(have-money)))))