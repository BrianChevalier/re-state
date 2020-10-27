(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [dynamic.pendulum :as p]
            [oz.core :as oz]
            [math.main :refer [sin cos]]))

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn plot [spec data]
  (deep-merge {:mark "line"}
              {:encoding {:x {:type "quantitative"}
                          :y {:type "quantitative"}}}
              spec
              {:data {:values (vec data)}}))

(def data (map (partial p/derived-state p/pendulum)
               (p/states p/pendulum)))

(defonce state (r/atom (cycle data)))

(defn circle [x y]
  [:circle {:cx x :cy y :r 0.1 :fill :white :stroke :black}])

(defn plane [system]
  (let [{:keys [phi]} system
        x1 (* -10 (cos phi))
        y1 (* -10 (sin phi))
        x2 (* 10  (cos phi))
        y2 (* 10  (sin phi))]
    [:line {:stroke :grey
            :x1 x1
            :y1 y1
            :x2 x2
            :y2 y2}]))

(defn draw-state [state]
  (let [{:keys [pendulum-hinge pendulum-tip system]} state]
    [:div
     [:div (pr-str (str (:t_n state)))]
     [:svg {:height "100vh" :width "100vw"}
      [:g {:transform "scale(10, -10) translate(10 -10)"}
       [plane system]
       [:circle {:cx 0 :cy 0 :r 0.3 :stroke :black}]
       [:line {:style {:stroke "red" :stroke-width "1"}
               :x1 (:x pendulum-hinge) :y1 (:y pendulum-hinge)
               :x2 (:x pendulum-tip) :y2 (:y pendulum-tip)}]
       [circle (:x pendulum-tip) (:y pendulum-tip)]
       [circle (:x pendulum-hinge) (:y pendulum-hinge)]]]]))

(defn update-loop []
  (swap! state #(or (rest %) data)))

(defn interval-loop []
  (js/setInterval #(update-loop) 10))

(defn plots []
  [:<>
   (for [vega (:plot p/pendulum)]
     [oz/vega-lite (plot vega (map (partial p/derived-state p/pendulum)
                                   (p/states p/pendulum)))])])

(defn app []
  [:div
   [:h1 "Driven Pendulum"]
   [plots]
   [draw-state (first @state)]])

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