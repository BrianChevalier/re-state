(ns app.styled
  (:require [app.colors :as c]))

(def selectors
  {:style        {:fn #(str "." %)}
   :style/hover  {:fn #(str "." % ":hover")}
   :style/small  {:fn #(str "." %) :query "@media (max-width: 42rem)"}
   :style/medium {:fn #(str "." %) :query "@media (min-width: 42rem) and (max-width: 64rem)"}
   :style/large  {:fn #(str "." %) :query "@media (min-width: 64rem)"}})

(defonce cache (atom {}))

(defn- value->css [v]
  (cond
    (number? v)  (str v "px")
    (keyword? v) (name v)
    :else        v))

(def exclude? #{:opacity :z-index})

(defn- style->css [style]
  (reduce-kv
   (fn [css k v]
     (str
      css
      (when (and k v)
        (str (value->css k) ":"
             (if (exclude? k)
               v
               (value->css v)) ";")))) "" style))

(defn- generate-class [selector style]
  (let [css (style->css style)]
    (when-not (empty? css)
      (let [k  (gensym)
            f  (-> selectors selector :fn)
            query (-> selectors selector :query)
            el (js/document.createElement "style")]
        (set! (.-innerHTML el)
              (if (nil? query)
                (str (f k) "{" css "}")
                (str query "{" (f k) "{" css "}" "}")))
        (.appendChild js/document.head el)
        (swap! cache assoc [selector style] k)
        k))))

(defn- get-class [selector style]
  (or (get @cache [selector style])
      (generate-class selector style)))

(defn- attrs->css [attrs]
  (reduce
   (fn [attrs selector]
     (if-not (contains? attrs selector)
       attrs
       (let [style (get attrs selector)
             class (get-class selector style)]
         (-> attrs
             (dissoc selector)
             (update :class str " " class)))))
   attrs
   (keys selectors)))

(defn styled [component defaults attrs & children]
  (into [component
         (if-not (map? attrs)
           attrs
           (attrs->css (merge defaults attrs)))]
        children))

(def a      (partial styled :a {}))
(def table  (partial styled :table {}))
(def tbody  (partial styled :tbody {}))
(def thead  (partial styled :thead {}))
(def tr     (partial styled :tr {}))
(def th     (partial styled :th {}))
(def td     (partial styled :td {}))
(def div    (partial styled :div {}))
(def span   (partial styled :span {}))
(def input  (partial styled :input {:style {:width "75px"
                                            :font-family "'Roboto Mono', Monospace"
                                            :font-size "0.9rem"
                                            :border :none
                                            :color (:number c/colors)
                                            :margin 0
                                            :background (:background2 c/colors)}}))
(def button (partial styled :button {:style {:padding "0px 10px"
                                             :background-color (:background2 c/colors)
                                             :font-family "'Roboto Mono', Monospace"
                                             :color :white
                                             :border :none}}))
(def img    (partial styled :img {}))