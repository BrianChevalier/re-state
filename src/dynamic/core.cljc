(ns dynamic.core
  (:refer-clojure :exclude [* + -])
  (:require [math.main :refer [abs]]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.linear :as lin]
            [clojure.core.matrix.operators :refer [* + -]]
            [dynamic.protocols :refer [valid-system? valid-state?]]))

(comment
  (lin/norm [1 2 3])
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
  (let [{:keys [t_0 x_0 v_0 mode K]} system
        a_0 ((:a_0 system) system)
        x_0 (cond
              #_(not (nil? mode)) #_(-> (K system)
                                    (lin/eigen)
                                    (:Q)
                                    (mat/get-column 1)
                                    (mat/matrix)
                                    (mat/reshape [(count x_0) 1])
                                    (mat/normalise))
              #_(mat/normalise
                                 (mat/reshape
                                  (mat/matrix
                                   (mat/get-column
                                    (:Q
                                     (lin/eigen
                                      (K system))) 1)) [(count x_0) 1]))
              (vector? x_0) (mat/reshape (mat/matrix x_0) [(count x_0) 1])
              :else x_0)
        v_0 (if (vector? v_0)
              (mat/reshape (mat/matrix v_0) [(count v_0) 1])
              v_0)
        state {:n 0
               :t_n t_0
               :x_n x_0
               :v_n v_0
               :a_n a_0
               :a_n+1 a_0}]
    state))

(def states
  (memoize
   (fn
     #_"returns lazy sequence of x, (f x), (f (f x))
   :neglect-state? decide whether to keep a state item"
     [system]
     (valid-system? system)
     (let [defaults {:t_0 0
                     :dt 0.01
                     :beta 0.5
                     :tolerance 1e-7
                     :method :GTR}
           system (merge defaults system)
           neglect-state? (fn [state] ;; which states to throw out
                            (not= 0 (rem (:n state) 5)))
           t_n<t_f        (fn [state] ;; when to stop iterating
                            (< (:t_n state)
                               (:t_f system)))]
       (->> (initial-state system)
            (iterate (partial new-state system))
            (take-while t_n<t_f)
            (remove neglect-state?)
            (map (fn [state]
                   (dissoc state :a_n+1))))))))


(comment
  #_(destruct)
  (-> #js [0 0 1]))