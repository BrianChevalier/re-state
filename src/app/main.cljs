(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            #_[reagent-forms.core :as forms]
            #_[dynamic.pendulum :as p]
            #_[math.main :refer [cos sin pi]]
            ;;[dynamic.doublependulum :as sys]
            [dynamic.nDoF :as sys]
            #_[oz.core :as oz]
            [dynamic.core :as dy]
            [math.matrix]))

(defonce system
  (r/atom sys/default-system))

(defonce state (r/atom (cycle (dy/states @system))))

(defn update-loop []
  (swap! state #(or (rest %) (cycle (dy/states @system)))))

(defn interval-loop []
  (js/setInterval #(update-loop) (* 0.01 5 1000)))

(defn number-input
  "This is a numeric input field with local state that only propogates
   model changes when the input is a valid number"
  [value _]
  (let [state (r/atom value)]
    (fn [_ on-change]
      [:td
       [:input {:style {:width "60px"
                        :font-family "'Roboto', sans-serif"
                        :font-size "0.9rem"}
                :value @state
                :type :number
                :on-change (fn [e]
                             (let [value (-> e .-target .-value)
                                   parsed (js/parseFloat value)]
                               (reset! state value)
                               (when-not (js/isNaN parsed)
                                 (on-change parsed))))}]])))

(defn vec-input [k ato]
  (let [values (k @ato)
        on-change #(swap! system assoc k %)]
    ^{:key k} 
    [:<>
     [:td [:label (str k)]]
     (map-indexed
      (fn [index value]
        ^{:key index}
        [:td [number-input value #(->> % (assoc values index) on-change)]])
      values)]))

(defn numeric-input [k ato]
  ^{:key k}
  [:<>
   [:td [:label (str k)]]
   [:td [number-input (k @ato) #(swap! system assoc k %)]]])

(defn control-table [params ato]
  [:table {:style
           {:border "3px solid grey"
            :padding "10px"
            :border-radius "10px"}}

   (for [param params]
     (let [input ({:numeric-vector vec-input
                   :numeric numeric-input} (:type param))]
       ^{:key (:key param)} [:tr [input (:key param) ato]]))
   
   [:tr [:td {:colspan "5"}
         [:button {:on-click #(reset! state (cycle (dy/states @ato)))
                   :style {:background-color "#4CAF50"
                           :color "white"
                           :border "none"
                           :border-radius "7px"
                           :text-align "center"
                           :font-size "1rem"
                           :padding "7px 20px"}}
          "Restart Simulation"]]]])

(defn app []
  [:div
   [:h1 (:title sys/metadata)]
   (:description sys/metadata)
   [:div {:style {:padding "20px"
                  :font-size "1.2rem"}}
    [control-table sys/controls system]]
   ;;[plots]
   [:div {:style {:border "2px solid black"
                  :border-radius "10px"}}
    [(:draw-state @system) @system (first @state)]]])

(defn render []
  (dom/render [app] (js/document.getElementById "root")))

(defn main!
  "Once per app"
  []
  (render)
  (interval-loop))

(defn reload!
  "On each reload, when the file is saved"
  []
  (render))

(comment
  (tap> @system))