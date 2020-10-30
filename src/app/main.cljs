(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [reagent-forms.core :as forms]
            #_[dynamic.pendulum :as p]
            [math.main :refer [cos sin pi]]
            [dynamic.doublependulum :as p2]
            #_[oz.core :as oz]
            [dynamic.core :as dy]
            [clojure.core.matrix :as core]
            [math.matrix]))

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

(def params (r/atom {}))

(def data
  (dy/states
   (p2/double-pendulum (merge {:L [10 10]
                               :W [1  1]
                               :k [0 0]
                               :P 1.15
                               :t_f 40
                               :beta 0.5
                               :gravity 9.81
                               :dt 0.01
                               :x_0 (core/matrix [[(/ pi 100)] [(/ pi 100)]])
                               :v_0 (core/matrix [[0] [0]])}
                              @params))))

(defonce state (r/atom (cycle data)))

(defn update-loop []
  (swap! state #(or (rest %) data)))

(defn interval-loop []
  (js/setInterval #(update-loop) 1))

#_(defn plots []
  [:<>
   (for [vega (:plot p/pendulum)]
     [oz/vega-lite (plot vega (map (partial p/derived-state p/pendulum)
                                   (dy/states p/pendulum)))])])

(defn row [label input]
  [:div
   [:div [:label label]]
   [:div input]])

(def form-template
  [:div
   (row "k1" [:input {:field :numeric :id :k1}])
   (row "k1" [:input {:field :numeric :id :k1}])
   (row "k2" [:input {:field :numeric :id :k2}])])

(defn form []
    (fn []
      [:div
       [forms/bind-fields form-template params]
       [:label (str @params)]]))

(defn app []
  [:div
   [:h1 " Pendulum"]
   [form]
   ;;[plots]
   [(:draw-state p2/benchmark) p2/benchmark (first @state)]])

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
