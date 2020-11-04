(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [dynamic.pendulum :as p]
            [dynamic.doublependulum :as double]
            [dynamic.nDoF :as nDoF]
            [dynamic.core :as dy]
            [dynamic.plots :as plots]
            [dynamic.draw :as d]
            [math.matrix]))

(def selections {:pendulum p/app-data
                 :nDoF nDoF/app-data
                 :double-pendulum double/app-data})

(defonce user-state (r/atom {:selection :double-pendulum
                             :animation-playing? false}))

(defonce system
  (r/atom (-> @user-state :selection selections :system)))

(defonce state (r/atom (cycle (dy/states @system))))

(defn update-loop []
  (if (:animation-playing? @user-state)
    (swap! state #(or (rest %) (cycle (dy/states @system))))
    nil))

(defn interval-loop []
  (js/setInterval #(update-loop) (* 0.01 5 1000)))

(defn number-input
  "This is a numeric input field with local state that only propogates
   model changes when the input is a valid number"
  [value _]
  (let [state (r/atom value)]
    (fn [_ on-change]
      [:td
       [:input {:style {:width "75px"
                        :font-family "'Roboto Mono', Monospace"
                        :font-size "0.9rem"
                        :border :none
                        :color (:number d/colors)
                        :background (:background2 d/colors)}
                :value @state
                :type :number
                :on-change (fn [e]
                             (let [value (-> e .-target .-value)
                                   parsed (js/parseFloat value)]
                               (reset! state value)
                               (when-not (js/isNaN parsed)
                                 (on-change parsed))))}]])))

(defn vec-input [param ato]
  (let [k (:key param)
        values (k @ato)
        on-change #(swap! system assoc k %)]
    ^{:key k}
    [:<>
     [:td {:style {:text-align :right
                   :color (:keyword d/colors)}}
      [:label [:code (str k)]]]
     (map-indexed
      (fn [index value]
        ^{:key index}
        [:td [number-input value #(->> % (assoc values index) on-change)]])
      values)]))

(defn numeric-input [param ato]
  (let [k (:key param)]
    ^{:key k}
    [:<>
     [:td {:style {:text-align :right :color (:keyword d/colors)}}
      [:label [:code (str k)]]]
     [:td [number-input (k @ato) #(swap! system assoc k %)]]]))

(defn select [param ato]
  (let [k (:key param)
        options (:options param)]
    ^{:key k}
    [:<>
     [:td (str k)]
     [:td
      [:select {:value (k @ato)
                :on-change (fn [e] (let [value (-> e .-target .-value keyword)]
                                     (swap! ato assoc k value)))}
       (for [option options]
         [:option {:value (:value option)} (:label option)])]]]))

(defn system-selection
  []
  [:select {:style {:background-color (:background2 d/colors)
                    :color (:string d/colors)
                    :border "1px solid transparent"
                    :padding "0.3rem"
                    :border-radius "0px"
                    :font-family "'Roboto Mono', Monospace"
                    :font-size "1rem"
                    :-webkit-appearance :none
                    :-moz-apperance :none}
            :value (-> @user-state :selection name)
            :on-change (fn [e]
                         (let [value (-> e .-target .-value keyword)]
                           (swap! user-state assoc :selection value)
                           (reset! system (-> selections value :system))
                           (reset! state (cycle (dy/states @system)))))}
   [:option {:value "double-pendulum"} "Double Pendulum"]
   [:option {:value "pendulum"} "Driven Pendulum"]
   [:option {:value "nDoF"} "Four DoF Oscillator"]])

(defn control-table [params ato]
  [:table {:style
           {:border (str "3px solid " (:border d/colors))
            :padding "10px"
            :border-radius "10px"}}
   [:tr
    [:td {:style
          {:text-align :right
           :color (:keyword d/colors)}}
     [:code "System"]]
    [:td {:colSpan 2} [system-selection]]]
   (for [param params]
     (let [input ({:numeric-vector vec-input
                   :numeric numeric-input
                   :select select} (:type param))]
       ^{:key (:key param)}
       [:tr [input param ato]]))])

(defn pause+play-button []
  [:button
   {:style {:padding "0px 10px"
            :background-color (:background2 d/colors)
            :font-family "'Roboto Mono', Monospace"
            :color :white
            :border :none}
    :on-click (fn []
                (swap! user-state assoc :animation-playing? (not (:animation-playing? @user-state))))}
   (if (:animation-playing? @user-state) "Pause" "Play")])

(defn restart-button []
  [:button
   {:style {:padding "0px 10px"
            :background-color (:background2 d/colors)
            :font-family "'Roboto Mono', Monospace"
            :color :white
            :border :none}
    :on-click #(reset! state (cycle (dy/states @system)))}
   "Restart"])

(defn app []
  (let [selection (-> @user-state :selection selections)]
    [:div {:style {:background (:background d/colors)}}
     [:div {:style {:padding "20px"
                    :background (:background2 d/colors)}}
      [:div {:style {:max-width "50rem"
                     :color (:text d/colors)
                     :margin "0 auto"
                     :opacity 1}}
       [:h1 {:style {:text-align :center}} "Dynamics Simulator"]
       [:p {:style {:padding "10px"}}
        "This is a classical dynamics simulator. The user interface is rendered
          using React via Reagent and ClojureScript. Linear Algebra operations are
          performed via an implementation of the clojure.core.matrix protocols
          using math.js. All operations are computed directly in the browser
          without a server-side component. All of the non-UI components work in 
          the JVM allowing for reuse of the same simulation code in a 
          high performance computing environment. "]]]
     ;;[:h1 (-> selection :metadata :title)]
     ;;(-> selection :metadata :description)
     [:div {:style {:border "1px solid transparent"
                    :max-width "100rem"
                    :display :grid
                    :grid-template-areas "\"controls animation\"
                                         \"plots plots\""
                    :grid-template-columns "1fr 3fr"
                    :grid-template-rows "1fr 0.5fr"}}
      
      [:div
       {:style {:grid-area "controls"
                :padding "20px"
                :font-size "1.2rem"}}
       [control-table (-> selection :controls) system]]
      
      [:div
       {:style {:padding "20px"
                :min-width "100px"
                :grid-area "animation"}}
       [:div
        {:style {:position :absolute
                 :display :flex
                 :padding-top "10px"
                 :padding-left "10px"}}
        [d/draw-time @system (first @state)]
        [pause+play-button]
        [restart-button]]
       
       [:div {:style {:max-height "500px" :max-width "700px"}}
        [(:draw-state @system) @system (first @state)]]]
    
       [plots/plots (-> selection :plots) (map (fn [state]
                                                 ((or (-> selection :derived-state) identity) @system state))
                                               (dy/states @system))]]]))

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