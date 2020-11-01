(ns dynamic.plots)

#_(defn plots []
    [:<>
     (for [vega (:plot p/pendulum)]
       [oz/vega-lite (plot vega (map (partial p/derived-state p/pendulum)
                                     (dy/states p/pendulum)))])])

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