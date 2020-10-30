(ns dynamic.core
  (:refer-clojure :exclude [* + -])
  (:require [math.main :refer [abs]]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.linear :as lin]
            [clojure.core.matrix.operators :refer [* + -]]))

(comment
  (+ (mat/matrix [[0 1]])
           (mat/matrix [[1 0]]))
  (+ 1 2))

(defn vnew [system state]
  (let [{:keys [beta dt]}       system
        {:keys [v_n a_n a_n+1]} state]
    (+ v_n (* dt (+ (* beta a_n)
                    (* (- 1 beta) a_n+1))))))

(defn xnew [system state]
  (let [{:keys [beta dt]} system
        {:keys [x_n v_n]} state]
    (+ x_n (* dt
              (+ (* beta v_n)
                 (* (- 1 beta) (vnew system state)))))))

(defn solve [g A]
  (cond
    (number? g) (/ g A)
    :else       (lin/solve (mat/matrix A)
                           (mat/matrix g))))

(defn error [g]
  (cond
    (number? g) (abs g)
    :else (lin/norm g)))

(defn new-state
  "Takes a system and state and returns a new state"
  [system state]
  (let [{:keys [residual tangent tolerance dt]} system
        {:keys [t_n a_n+1]}                     state
        v_n+1 (vnew system state)
        x_n+1 (xnew system state)
        t_n+1 (+ t_n dt)
        new (assoc state
                   :v_n+1 v_n+1
                   :x_n+1 x_n+1
                   :t_n+1 t_n+1)
        g     (residual system new)
        A     (tangent system new)
        a_n+1 (- a_n+1 (solve g A))
        newton-it (inc (get state :newton-it 0))]
    (cond
      (> newton-it 30)
      (throw (js/Error. "failed to converge"))

      ;; swap state forward for next iteration
      (< (error g) tolerance)
      {:a_n a_n+1 :a_n+1 a_n+1 ;;initial guess for next iteration
       :v_n v_n+1
       :x_n x_n+1
       :t_n t_n+1
       :n (inc (:n new))}

      :else
      (recur system (assoc new :a_n+1 a_n+1 :newton-it newton-it)))))

(defn initial-state [system]
  (let [{:keys [t_0 x_0 v_0]} system
        a_0 ((:a_0 system) system)
        state {:n 0
               :t_n t_0
               :x_n x_0
               :v_n v_0
               :a_n a_0
               :a_n+1 a_0}]
    state))

(defn states
  "returns lazy sequence of x, (f x), (f (f x))
   :neglect-state? decide whether to keep a state item"
  [system]
  (let [defaults {:t_0 0
                  :dt 0.01
                  :beta 0.5
                  :tolerance 1e-7
                  :method :GTR}
        system (merge defaults system)
        neglect-state? (fn [state] ;; which states to throw out
                         (not= 0 (rem (:n state) 1)))
        t_n<t_f        (fn [state] ;; when to stop iterating
                         (< (:t_n state)
                            (:t_f system)))]
    (->> (initial-state system)
         (iterate (partial new-state system))
         (take-while t_n<t_f)
         (remove neglect-state?)
         (map (fn [state]
                (dissoc state :a_n+1))))))


(comment
  #_(destruct)
  (-> #js [0 0 1]))