(ns dynamic.pendulum)

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn abs [x] (Math/abs x))
(defn ** [x] (* x x))

(defn residual [system state]
  (let [{:keys [Xo Omega gravity L phi]} system
        {:keys [t_n+1 x_n+1 a_n+1]}      state
        a (* (/ -3 2) (/ (* Xo Omega Omega) L))
        b (* (sin (* Omega t_n+1)) (cos (- x_n+1 phi)))
        c (* (/ 3 2) (/ gravity L) (sin x_n+1))]
    (+ a_n+1 (* a b) c)))

(defn tangent [system state]
  (let [{:keys [Xo Omega L phi gravity dt beta]} system
        {:keys [t_n+1 x_n+1]}                    state
        zeta (* (** dt) (** (- 1 beta)))
        a (* (/ 3 2) (/ (* Xo Omega Omega) L))
        b (* (sin (* Omega t_n+1)) (sin (- x_n+1 phi)))
        c (* (/ 3 2) (/ gravity L) (cos x_n+1))]
    (+ 1 (* zeta (+ (* a b) c)))))

(defn a_0 [system]
  (let [{:keys [t_0 x_0 phi gravity L Omega Xo]} system
        a (* (/ 3 2) (/ (* Xo Omega Omega) L))
        b (* (sin (* Omega t_0)) (sin (- x_0 phi)))
        c (* (/ 3 2) (/ gravity L) (cos x_0))]
    (- (* a b) c)))

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
        new (merge state {:v_n+1 (vnew system state)
                          :x_n+1 (xnew system state)
                          :t_n+1 (+ t_n dt)})
        g     (residual system new)
        A     (tangent system new)
        a_n+1 (- a_n+1 (/ g A))]
    (if (< (abs g) tolerance)
      ;; swap state forward for next iteration
      {:a_n a_n+1 :a_n+1 a_n+1 ;;initial guess for next iteration
       :v_n (:v_n+1 new)
       :x_n (:x_n+1 new)
       :t_n (:t_n+1 new)
       :n (inc (:n new))}
      (new-state system (merge new {:a_n+1 a_n+1})))))

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
                            (:t_f system)))
        write-state    (fn [state] ;; hook to change what's stored in each state
                         (dissoc state :a_n+1))]
    (->> (initial-state system)
         (iterate (partial new-state system))
         (take-while t_n<t_f)
         (remove neglect-state?)
         (map write-state))))

(def pendulum
  {:gravity  9.81
   :mass     2.0
   :L        1
   :phi      0
   :Omega    1
   :Xo       0
   :x_0      0;;(/ (Math/PI) 2)
   :v_0      0
   :t_0      0
   :t_f      20
   :dt       0.01
   :beta     0.5
   :tolerance 1e-7
   :integration-method :GTR
   :tangent  tangent
   :residual residual
   :a_0      a_0})

(comment
  (require '[portal.api :as p])
  (p/open)
  (p/tap)
  (p/clear)
  (tap> (states pendulum)))