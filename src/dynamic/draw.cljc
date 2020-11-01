(ns dynamic.draw
  (:require [math.main :refer [sin cos]]))

(defn canvas [view-box & content]
  ;; x, y, width, height
  [:center
   [:svg {:view-box view-box
          :width "90%" :height "auto"
          :preserveAspectRatio "xMidYMid meet"}
    (into [:g ;;translate (10 -50)
           {:transform "scale(1, -1)"}] content)]])

(defn circle
  ([m]
   (circle (:x m) (:y m)))
  ([x y & rest]
   [:circle (merge
             {:cx x
              :cy y
              :r 0.1
              :fill "red"
              :stroke "black"}
             rest)]))

(defn line [start end]
  [:line {:stroke :grey
          :stroke-width "1"
          :x1 (:x start)
          :y1 (:y start)
          :x2 (:x end)
          :y2 (:y end)}])

(defn plane
  ([angle]
   (plane angle {:scale 1}))
  ([angle m]
   (let [scale (:scale m)
         x1 (* -1 scale (cos angle))
         y1 (* -1 scale (sin angle))
         x2 (* scale  (cos angle))
         y2 (* scale  (sin angle))]
     [:line {:stroke :grey
             :x1 x1
             :y1 y1
             :x2 x2
             :y2 y2}])))

(defn draw-time [_ state]
  [:div (str (.toFixed (or (:t_n state) 0) 2))])

(defn pendulum [hinge tip]
  [:<>
   [line hinge tip]
   [circle tip]
   [circle hinge]])

(defn double-pendulum [hinge tip1 tip2]
    [:<>
     [pendulum hinge tip1]
     [pendulum tip1 tip2]])