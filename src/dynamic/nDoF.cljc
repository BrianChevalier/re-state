(ns dynamic.nDoF
  (:refer-clojure :exclude [* + -])
  (:require [clojure.core.matrix :as core]
            [math.main :refer [**]]
            [clojure.core.matrix.operators :refer [* + -]]
            [math.matrix]
            [dynamic.draw :as d]))


(defn M [system]
  (let [mass (:mass system)]
    (core/diagonal-matrix mass)))

(defn K [system]
  (let [[k1 k2 k3 k4] (:k system)]
    (core/matrix [[(+ k1 k2) (- k2)    0         0]
                  [(- k2)    (+ k2 k3) (- k3)    0]
                  [0         (- k3)    (+ k3 k4) (- k4)]
                  [0         0         (- k4)    k4]])))

(defn zeta [system]
  (let [{:keys [dt beta]} system]
    (** (* dt (- 1 beta)))))

(defn residual [system state]
  (let [{:keys [amplitudes frequencies]} system
        {:keys [t_n+1 x_n+1 a_n+1]} state
        g (- (+ (core/mmul (M system) a_n+1)
                (core/mmul (K system) x_n+1))
             (* (core/reshape (core/matrix amplitudes) #js [4 1])
                (core/sin (* (core/reshape (core/matrix frequencies) #js [4 1]) t_n+1))))]
    g))

(defn tangent [system _]
  (+ (M system)
     (* (zeta system) (K system))))

(defn a_0 [system]
  (let [{:keys [x_0]} system
        x_0 (core/reshape (core/matrix x_0) #js [(count x_0) 1])]
    (core/mmul (core/inverse (M system))
               (core/mmul (K system) x_0))))

(defn spring [start end dx]
  (let [color (if (= 0 dx) "black"
                  (if (neg? dx) "blue" "red"))
        x1 (:x start)
        y1 (:y start)
        x2 (:x end)
        y2 (:y end)
        path (str "M" x1 " " y1
                  "L" x2 " " y2)]
    [:path {:d path
            :stroke color
            :stroke-width "0.4"
            ;;:stroke-linecap "round"
            :stroke-linejoin "round"
            :fill "none"}]))

(defn rect-from-center [center width height]
  [:rect {:x (- (:x center) (/ width 2))
          :y (- (:y center) (/ height 2))
          :width width
          :height height
          :rx 0.5
          :style {:fill :grey}}])

(defn draw-state [system state]
  (let [L (reductions + (:L system)) ;;cumulative sum
        x (:x_n state)
        uuids (range 4)
        scale (or (:scale system) 10)
        width (* 1.2 (reduce + (:L system)))]
    [:div
     [:div (.toFixed (or (:t_n state) 0) 2)]
     [d/canvas (str "0 -5 " width " 10") ;;"0 -5 25 10"
      (for [[L-1 [x-1] L [x] uuid] (map vector (concat [0] L) (concat [[0]] x) L x uuids)]
        ^{:key (str uuid "rect")}
        [:<>
         [rect-from-center {:x (+ L (* scale x)) :y 0} 3 3]
         [spring
          {:x (+ L-1 (* scale x-1)) :y 0}
          {:x (+ L (* scale x)) :y 0} (- x x-1)]])
      (for [[L [x] uuid] (map vector L x uuids)]
        ^{:key (str uuid "circle")}
        [:circle {:cx (+ L (* scale x)) :cy 0 :r 0.35 :fill "white" :stroke "black" :stroke-width "0.1"}])]]))

(def benchmark
  {:x_0 [0.0516 0.0516 0 -0.0516]
   :v_0 [0 0 0 0]
   :t_f 5
   :dt 0.01
   :beta 0.5
   :amplitudes [0 0 0 0]
   :frequencies [1.098 1.098 1.098 1.098]
   :mass [5 5 5 5]
   :k [50 50 50 50]
   :L [5 5 5 5]
   :residual residual
   :tangent tangent
   :a_0 a_0
   :draw-state draw-state})

(def default-system
  {:x_0 [0.0516 0.0516 0 -0.0516]
   :v_0 [0 0 0 0]
   :t_f 5
   :dt 0.01
   :beta 0.5
   :amplitudes [0 0 0 0]
   :frequencies [1.098 1.098 1.098 1.098]
   :mass [5 5 5 5]
   :k [50 50 50 50]
   :L [5 5 5 5]
   :scale 10
   :residual residual
   :tangent tangent
   :a_0 a_0
   :draw-state draw-state})

(def controls
  [{:key :x_0 :type :numeric-vector}
   {:key :v_0 :type :numeric-vector}
   {:key :k :type :numeric-vector}
   {:key :L :type :numeric-vector}
   {:key :mass :type :numeric-vector}
   {:key :amplitudes :type :numeric-vector}
   {:key :frequencies :type :numeric-vector}
   {:key :scale :type :numeric}
   {:key :t_f :type :numeric}
   {:key :dt :type :numeric}
   {:key :beta :type :numeric}])

(def metadata
  {:title
   "Four Degree of Freedom System"
   :description
   [:div
    [:p "This is a four Degree of Freedom (DoF) system"]
    [:p "Red: Tension, Blue: compression, Black: neither"]]})
