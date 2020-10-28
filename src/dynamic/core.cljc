(ns dynamic.core
  (:require [math.main :refer [** abs]]))


(defn bn [system state]
  (let [{:keys [beta dt]}     system
        {:keys [x_n v_n a_n]} state]
    (+ x_n
       (* v_n dt)
       (* (** dt)
          beta
          (- 1 beta)
          a_n))))

(defn vnew [system state]
  (let [{:keys [beta dt]}       system
        {:keys [v_n a_n a_n+1]} state]
    (+ v_n (* dt (+ (* beta a_n)
                    (* (- 1 beta) a_n+1))))))

(defn xnew [system state]
  (let [{:keys [beta dt]} system
        {:keys [x_n v_n]} state]
    (+ x_n (* dt (+ (* beta v_n)
                    (* (- 1 beta) (vnew system state)))))))

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
        a_n+1 (- a_n+1 (/ g A))]
    (if (< (abs g) tolerance)
      ;; swap state forward for next iteration
      {:a_n a_n+1 :a_n+1 a_n+1 ;;initial guess for next iteration
       :v_n v_n+1
       :x_n x_n+1
       :t_n t_n+1
       :n (inc (:n new))}
      (recur system (assoc new :a_n+1 a_n+1)))))

(defn initial-state [system]
  (let [{:keys [t_0 x_0 v_0]} system
        a_0 ((:a_0 system) system)]
    {:n 0
     :t_n t_0
     :x_n x_0
     :v_n v_0
     :a_n a_0
     :a_n+1 a_0}))

(defn states
  "returns lazy sequence of x, (f x), (f (f x))
   :neglect-state? decide whether to keep a state item
   :write-state a function that takes in a state map and
                returns the new representation of that state. i.e., 
                is there something that needs to be computed based 
                on state such as energy"
  [system]
  (let [neglect-state? (fn [state] ;; which states to throw out
                         (not= 0 (rem (:n state) 5)))
        t_n<t_f        (fn [state] ;; when to stop iterating
                         (< (:t_n state)
                            (:t_f system)))]
    (->> (initial-state system)
         (iterate (partial new-state system))
         (take-while t_n<t_f)
         (remove neglect-state?)
         (map (fn [state]
                (dissoc state :a_n+1))))))