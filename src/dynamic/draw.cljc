(ns dynamic.draw
  (:require [math.main :refer [sin cos]]))

(def colors
  #_{:string "#859900"
   :keyword "#268bd2"
   :exception "#dc322f"
   :text "#93a1a1"
   :background2 "#eee8d5"
   :boolean "#268bd2"
   :tag "#b58900"
   :uri "#cb4b16"
   :namespace "#2aa198"
   :diff-add "#859900"
   :package "#2aa198"
   :border "#839496"
   :symbol "#93a1a1"
   :diff-remove "#dc322f"
   :number "#d33682"
   :background "#fdf6e3"}


  #_{:string "#859900"
   :keyword "#268bd2"
   :exception "#dc322f"
   :text "#93a1a1"
   :background2 "#002b36"
   :boolean "#268bd2"
   :tag "#b58900"
   :uri "#cb4b16"
   :namespace "#2aa198"
   :diff-add "#859900"
   :package "#2aa198"
   :border "#586e75"
   :symbol "#93a1a1"
   :diff-remove "#dc322f"
   :number "#d33682"
   :background "#073642"}

  {:string "#a3be8c"
   :keyword "#5e81ac"
   :exception "#bf616a"
   :text "#d8dee9"
   :background2 "#2a2e39"
   :boolean "#5e81ac"
   :tag "#ebcb8b"
   :uri "#d08770"
   :namespace "#88c0d0"
   :diff-add "#a3be8c"
   :package "#88c0d0"
   :border "#4c566a"
   :symbol "#d8dee9"
   :diff-remove "#bf616a"
   :number "#b48ead"
   :background "#2e3440"})

(defn canvas [view-box & content]
  ;; x, y, width, height
  [:div {:style {:border (str "3px solid " (:border colors))
                 :border-radius "10px"}}
   [:center
    [:svg {:display :inline-block
           :view-box view-box
           ;;:width "auto%"
           :style {:max-height "500px" :max-width "700px"}
           :preserveAspectRatio "xMidYMid meet"}
     (into [:g ;;translate (10 -50)
            {:transform "scale(1, -1)"}
            [:line {:stroke-width "0.1px"
                    :stroke :grey
                    :stroke-dasharray "1 1 1 1"
                    :x1 -100 :y1 0
                    :x2 100 :y2 0}]
            [:line {:stroke-width "0.1px"
                    :stroke :grey
                    :stroke-dasharray "1 1 1 1"
                    :x1 0 :y1 100
                    :x2 0 :y2 -100}]]
           content)]]])

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
          :stroke-width "0.75"
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
             :stroke-width "0.5"
             :x1 x1
             :y1 y1
             :x2 x2
             :y2 y2}])))

(defn draw-time [_ state]
  [:div {:style {:color (:number colors)
                 ;;:background-color (:background2 colors)
                 :font-size "1rem"
                 :font-family "'Roboto Mono', Monospace"}}
   (str (.toFixed (or (:t_n state) 0) 2))])

(defn pendulum [hinge tip]
  [:<>
   [line hinge tip]
   [circle tip]
   [circle hinge]])

(defn double-pendulum [hinge tip1 tip2]
    [:<>
     [pendulum hinge tip1]
     [pendulum tip1 tip2]])

(def undo-alt
  [:svg
   {:height "1rem"
    :width "auto"
    :aria-hidden :true
    :focusable :false
    :data-prefix "fas"
    :data-icon "undo-alt"
    :class "svg-inline--fa fa-undo-alt fa-w-16"
    :role "img"
    :xmlns "http://www.w3.org/2000/svg"
    :viewBox "0 0 512 512"}
  [:path {:fill "currentColor"
          :d "M255.545 8c-66.269.119-126.438 26.233-170.86 68.685L48.971 40.971C33.851 25.851 8 36.559 8 57.941V192c0 13.255 10.745 24 24 24h134.059c21.382 0 32.09-25.851 16.971-40.971l-41.75-41.75c30.864-28.899 70.801-44.907 113.23-45.273 92.398-.798 170.283 73.977 169.484 169.442C423.236 348.009 349.816 424 256 424c-41.127 0-79.997-14.678-110.63-41.556-4.743-4.161-11.906-3.908-16.368.553L89.34 422.659c-4.872 4.872-4.631 12.815.482 17.433C133.798 479.813 192.074 504 256 504c136.966 0 247.999-111.033 248-247.998C504.001 119.193 392.354 7.755 255.545 8z"}]])