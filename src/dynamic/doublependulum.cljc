(ns dynamic.doublependulum
  (:require
   [math.main :refer [** cos sin pi]]
   [clojure.core.matrix :as core]
   [clojure.core.matrix.linear :as lin]
   [dynamic.draw :as d]
   [dynamic.core :as dy]
   [math.matrix]))

(comment
  (core/matrix [[0 0] [1 1]])
  (vec (core/matrix [[0 0] [0 0]]))
  (lin/solve (core/matrix [[-2, 3], [2, 1]])
             (core/matrix [11, 9]))
  (core/e* 3 (core/matrix [[1 1] [1 1]])))

(defn any-nil? [values]
  (if (some nil? values)
    (throw (js/Error. "Nil value in collection!"))))

(defn const [system]
  (let [{:keys [L W gravity P beta dt]} system
        [L1 L2] L
        [W1 W2] W
        eta (* dt (- 1 beta))
        ;;_ (any-nil? [L1 L2 gravity W1 W2 P beta dt eta beta])
        ]
    {:a (* L1
           (+ (* (/ 1 2) W1) W2 P))
     :b (* L2
           (+ (* (/ 1 2) W2) P))
     :c (* (/ (** L1) gravity)
           (+ (* (/ 1 3) W1) W2))
     :d (* (/ 1 2)
           (/ (* L1 L2) gravity)
           W2)
     :e (* (/ 1 3)
           (/ (** L2) gravity)
           W2)
     :eta eta
     :zeta (** eta)}))

(def constants (memoize const))

(defn residual [system state]
  (let [{:keys [k]}             system
        {:keys [a b c d e]}         (-> system constants)
        [k1 k2] k
        {:keys [x_n+1 v_n+1 a_n+1]} state
        [[x1] [x2]] x_n+1
        [[v1] [v2]] v_n+1
        [[a1] [a2]] a_n+1
        x2-x1 (- x2 x1)
        cosx2-x1 (cos x2-x1)
        sinx2-x1 (sin x2-x1)
        g1 (+ (* c a1)
              (* d cosx2-x1 a2)
              (* -1 d (** v2) sinx2-x1)
              (* -1 a (sin x1))
              (* x1 (+ k1 k2))
              (* -1 k2 x2))
        g2 (+ (* e a2)
              (* d cosx2-x1 a1)
              (* d (** v1) sinx2-x1)
              (* -1 b (sin x2))
              (* -1 k2 x1)
              (* k2 x2))]
    [[g1]
     [g2]]))


(defn tangent [system state]
  (let [{:keys [a b c d e zeta eta]}  (-> system constants)
        {:keys [k]}                 system
        [k1 k2] k
        {:keys [x_n+1 v_n+1 a_n+1]} state
        [[x1] [x2]] x_n+1 ;;[x1 x2] [[x1] [x2]]
        [[v1] [v2]] v_n+1
        [[a1] [a2]] a_n+1
        x2-x1 (- x2 x1)
        cosx2-x1 (cos x2-x1)
        sinx2-x1 (sin x2-x1)
        A11 (+ c
               (* zeta d sinx2-x1 a2)
               (* zeta d (** v2) cosx2-x1)
               (* -1 zeta a (cos x1))
               (* zeta (+ k1 k2)))
        A12 (+ (* d cosx2-x1)
               (* -1 zeta d sinx2-x1 a2)
               (* -2 eta d v2 sinx2-x1)
               (* -1 zeta d (** v2) cosx2-x1)
               (* -1 zeta k2))
        A21 (+ (* d cosx2-x1)
               (* zeta d sinx2-x1 a1)
               (* 2 eta d v1 sinx2-x1)
               (* -1 zeta d (** v1) cosx2-x1)
               (* -1 zeta k2))
        A22 (+ e
               (* -1 zeta d sinx2-x1 a1)
               (* zeta d (** v1) cosx2-x1)
               (* -1 zeta b (cos x2))
               (* zeta k2))]
    [[A11 A12]
     [A21 A22]]))


(defn a_0 [system]
  (let [{:keys [a b c d e]} (-> system constants)
        {:keys [k x_0 v_0]}   system
        [k1 k2] k
        [x1 x2] x_0
        [v1 v2] v_0
        cosx2-x1 (cos (- x2 x1))
        sinx2-x1 (sin (- x2 x1))
        A11 c
        A12 (* d cosx2-x1)
        A21 (* d cosx2-x1)
        A22 e
        g1 (+ (* -1 d (** v2) sinx2-x1)
              (* -1 a (sin x1))
              (* x1 (+ k1 k2))
              (* -1 k2 x2))
        g2 (+ (* d (** v1) sinx2-x1)
              (* -1 b (sin x2))
              (* -1 k2 x1)
              (* k2 x2))
        g [[g1]
           [g2]]
        A [[A11 A12]
           [A21 A22]]
        a_0 (lin/solve (core/matrix A)
                       (core/matrix g))
        _ (any-nil? [A11 A12 A21 A22 k x_0 v_0 g1 g2 a_0])]
    a_0))


(defn draw-state [system state]
  (let [[L1 L2] (:L system)
        [[x1] [x2]] (:x_n state)
        tip1 {:x (* L1 (sin x1))
              :y  (* L1 (cos x1))}
        tip2 {:x (+ (* L1 (sin x1)) (* L2 (sin x2)))
              :y (+ (* L1 (cos x1)) (* L2 (cos x2)))}
        width (* 2.2 (+ L1 L2))
        position (* -1 (/ width 2))
        ]
    [:div
     [d/canvas (str position " " position " " width " " width)
      [d/circle {:x 0 :y 0}]
      [d/double-pendulum {:x 0 :y 0} tip1 tip2]]]))

(defn double-pendulum
  ":L Lenth
   :W [1  1 ]
   :k [50 50]
   :P 1
   :dt 0.01
   :tf 40
   :x_0 [(/ pi 100) (/ pi 100)]
   :v_0 [0 0]"
  [system]
  (assoc system
         :residual residual
         :tangent tangent
         :a_0 a_0
         :draw-state draw-state))

(def benchmark
  (double-pendulum
   {:L [10 10]
    :W [1  1]
    :k [0 0]
    :P 1.15
    :t_f 40
    :beta 0.5
    :gravity 9.81
    :dt 0.01
    :x_0 (core/matrix [[(/ pi 100)] [(/ pi 100)]])
    :v_0 (core/matrix [[0] [0]])}))

(def default-system
  {:L [10 10]
   :W [1  1]
   :k [50 50]
   :P 1.0
   :t_f 40
   :beta 0.5
   :gravity 9.81
   :dt 0.01
   :x_0 [(/ pi 100) (/ pi 100)]
   :v_0 [0 0]
   :residual residual
   :tangent tangent
   :a_0 a_0
   :draw-state draw-state})

(def controls
  [{:key :x_0 :type :numeric-vector}
   {:key :v_0 :type :numeric-vector}
   {:key :k :type :numeric-vector}
   {:key :L :type :numeric-vector}
   {:key :W :type :numeric-vector}

   {:key :gravity :type :numeric}
   {:key :P :type :numeric}
   {:key :t_f :type :numeric}
   {:key :dt :type :numeric}
   {:key :beta :type :numeric}])

(def metadata
  {:title
   "Inverted Double Pendulum"
   :description
   [:p "This is a two degree of freedom inverted double pendulum."]})

(def app-data
  {:controls controls
   :metadata metadata
   :system default-system})