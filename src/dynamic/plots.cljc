(ns dynamic.plots
  (:require [oz.core :as oz]
            [dynamic.draw :as d]))

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn plot
  "Provides default values for vega plots"
  [spec data]
  (deep-merge
   {:title {:color (:text d/colors)}}
   {:config
    {:axis {:domainColor (:border d/colors)
            :tickColor (:border d/colors)
            :gridColor (:border d/colors)
            :titleColor (:text d/colors)
            :labelColor (:text d/colors)}}}
   {:background (:background d/colors)}
   {:mark "line"}
   {:encoding {:x {:type "quantitative"}
               :y {:type "quantitative"}}}
   spec
   {:data {:values (vec data)}}))

(defn plots [specs states]
  [:<>
   (for [spec specs]
     ^{:key (hash spec)}
     [:div
      [oz/vega-lite (plot spec states)]])])