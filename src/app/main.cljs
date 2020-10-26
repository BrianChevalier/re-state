(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [dynamic.pendulum :as p]))

(def data (map (partial p/derived-state p/pendulum)
               (p/states p/pendulum)))

(defonce state (r/atom (cycle data)))

(defn transform
  [num]
  (+ 50 (* -50 num)))

(defn circle [x y]
  [:circle {:cx (transform x) :cy (transform y) :r 5 :fill :white :stroke :black}])

(defn plane [system]
  (let [{:keys [phi]} system
        x1 (* 10)
        y1 
        x2
        y2]
  [:line {:x1
          :y1
          :x2
          :y2}]))

(defn draw-state [state]
  (let [{:keys [pendulum-hinge pendulum-tip system]} state]
    [:div
     [:div (pr-str (:t_n state))]
     [:svg {:height "100vh" :width "100vw"}
      [:circle {:cx (transform 0) :cy (transform 0) :r 5 :fill :white :stroke :black}]
      ;;[:line ]
      [:line {:style {:stroke "red" :stroke-width "5"}
              :x1 (transform (:x pendulum-hinge)) :y1 (transform (:y pendulum-hinge))
              :x2 (transform (:x pendulum-tip)) :y2 (transform (:y pendulum-tip))}]
      [circle (:x pendulum-tip) (:y pendulum-tip)]
      [circle (:x pendulum-hinge) (:y pendulum-hinge)]]]))

(defn update-loop []
  (swap! state #(or (rest %) data)))

(defn interval-loop []
  (js/setInterval #(update-loop) 90))

(defn app []
  [draw-state (first @state)])

(defn render []
  (dom/render [app]
              (js/document.getElementById "root")))

(defn main!
  "Once per app"
  []
  (render)
  (interval-loop))

(defn reload!
  "On each reload, when the file is saved"
  []
  (render))