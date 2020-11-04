(ns dynamic.pendulum
  (:require [math.main :refer [sin cos ** pi]]
            [dynamic.core :as dy]
            [dynamic.draw :as d]))


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
        b (* (sin (* Omega t_0)) (cos (- x_0 phi)))
        c (* (/ 3 2) (/ gravity L) (sin x_0))]
    (- (* a b) c)))

(defn potential [system state]
  (let [{:keys [phi Omega w L Xo]} system
        {:keys [t_n x_n]}          state
        a (* w L Xo
             (sin (* Omega t_n))
             (sin phi))
        b (* (/ 1 2) w L L (cos x_n))
        Uo 100]
    (+ (- a b) Uo)))

(defn kinetic [system state]
  (let [{:keys [w gravity Xo Omega phi L]} system
        {:keys [t_n x_n v_n]} state
        a (* (/ 1 2) (/ w gravity) L
             (** Xo) (** Omega) (** (cos (* Omega t_n))))
        b (* (/ 1 2) (/ w gravity) (** L) v_n Xo Omega
             (cos (* Omega t_n)) (cos (- x_n phi)))
        c (* (/ 1 6) (/ w gravity) L L L v_n v_n)]
    (+ a b c)))


(defn line-plot [x-key y-key]
  {:mark :line
   :encoding {:order {:field :n}
              :x {:field x-key :type :quantitative}
              :y {:field y-key :type :quantitative}}})

(defn derived-state [system state]
  (let [{:keys [L Xo Omega phi]} system
        {:keys [t_n x_n]} state
        x (* Xo (sin (* Omega t_n)) (cos phi))
        y (* Xo (sin (* Omega t_n)) (sin phi))
        T (kinetic system state)
        U (potential system state)]
    (assoc state
           ;;:system system
           :pendulum-hinge {:x x
                            :y y}
           :pendulum-tip {:x (+ x (* L (sin x_n)))
                          :y (+ y (* -1 L (cos x_n)))}
           :total-energy (+ T U)
           :kinetic T
           :potential U)))

(defn draw-state [system state]
  (let [{:keys [t_n x_n]} state
        {:keys [L Xo Omega phi]} system
        x (* Xo (sin (* Omega t_n)) (cos phi))
        y (* Xo (sin (* Omega t_n)) (sin phi))
        pendulum-hinge {:x x :y y}
        pendulum-tip {:x (+ x (* L (sin x_n)))
                      :y (+ y (* -1 L (cos x_n)))}
        width (* 2.2 L)
        position (* -1 (/ width 2))]
    [:div
     [d/canvas (str position " " position " " width " " width)
      [d/plane (:phi system) {:scale 10}]
      [d/circle {:x 0 :y 0}]
      [d/pendulum pendulum-hinge pendulum-tip]]]))

(def pendulum
  {:gravity  9.81
   :mass     2.0
   :w        2
   :L        10
   :phi      (/ pi 3)
   :Omega    1.213;;(sqrt (/ (* 1.5 9.81) 5))
   :Xo       2.1
   :x_0      0 ;;(/ pi 2)
   :v_0      0
   :t_f      100
   :dt       0.01
   :beta     0.5
   :tolerance 1e-7
   :tangent  tangent
   :residual residual
   :a_0      a_0
   :draw-state draw-state})

(def default-system
  {:gravity  9.81
   :mass     2.0
   :w        2
   :L        10
   :phi      (/ pi 3)
   :Omega    1.213;;(sqrt (/ (* 1.5 9.81) 5))
   :Xo       2.1
   :x_0      0 ;;(/ pi 2)
   :v_0      0
   :t_f      60
   :dt       0.01
   :beta     0.5
   :tolerance 1e-7
   :tangent  tangent
   :residual residual
   :a_0      a_0
   :draw-state draw-state})

(def controls
  [{:key :x_0 :type :numeric}
   {:key :v_0 :type :numeric}

   {:key :L :type :numeric}
   {:key :w :type :numeric}
   {:key :Xo :type :numeric}
   {:key :phi :type :numeric}
   {:key :Omega :type :numeric}
   {:key :gravity :type :numeric}
   {:key :t_f :type :numeric}
   {:key :dt :type :numeric}
   {:key :beta :type :numeric}])

(def metadata
  {:title
   "Driven Pendulum"
   :description
   [:div
    [:p "This "]]})

(def plots [{:title {:text "Energy vs. time"}
             :layer [(line-plot :t_n :kinetic)
                     (line-plot :t_n :potential)
                     (line-plot :t_n :total-energy)]}
            {:title {:text "Phase Portrait"}
             :mark :line
             :layer [(line-plot :x_n :v_n)]}
            {:title {:text "Angle vs. time"}
             :mark :line
             :encoding {:x {:field :t_n :axis {:title "Time (s)"}}
                        :y {:field :x_n :axis {:title "theta (rad)"}}}}
            {:title {:text "Angular velocity vs. time"}
             :mark :line
             :encoding {:x {:field :t_n :axis {:title "Time (s)"}}
                        :y {:field :v_n :axis {:title "velocity (rad/s)"}}}}])

(def app-data {:system default-system
               :controls controls
               :metadata metadata
               :plots plots
               :derived-state derived-state})

(comment
  ;;plotting
  (require '[oz.core :as oz])
  (oz/start-server!)

  (defn deep-merge [a & maps]
    (if (map? a)
      (apply merge-with deep-merge a maps)
      (apply merge-with deep-merge maps)))

  (defn plot [spec data]
    (deep-merge ;;{:mark "line"}
     {:encoding {:x {:type "quantitative"}
                 :y {:type "quantitative"}}}
     spec
     {:data {:values (vec data)}}))

  (oz/view! [:div
             #_[:ul (for [[k v] pendulum] [:li (str k v)])]
             (for [vega (:plot pendulum)]
               [:vega-lite (plot vega (map (partial derived-state pendulum)
                                           (dy/states pendulum)))])]))