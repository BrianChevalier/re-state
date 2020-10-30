(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            #_[dynamic.pendulum :as p]
            [math.main :refer [cos sin]]
            [dynamic.doublependulum :as p2]
            [oz.core :as oz]
            [dynamic.core :as dy]
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

(def L1 (first (:L p2/benchmark)))
(def L2 (second (:L p2/benchmark)))

(defn tip1 [state]
  (let [[[x1] _] (:x_n state)]
    {:x (* L1 (sin x1))
    :y  (* L1 (cos x1))}))

(defn tip2 [state]
  (let [[[x1] [x2]] (:x_n state)]
    {:x (+ (* L1 (sin x1)) (* L2 (sin x2)))
     :y (+ (* L1 (cos x1)) (* L2 (cos x2)))}))

(def data
    (map #(assoc %
                :pendulum-hinge {:x 0 :y 0}
                :pendulum-tip1 (tip1 %)
                :pendulum-tip2 (tip2 %)) (dy/states p2/benchmark)))

(comment
  (-> data second)
  (-> data)
  )

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

(defn app []
  [:div
   [:h1 " Pendulum"]
   ;;[plots]
   [(:draw-state p2/benchmark) (first @state)]])

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
