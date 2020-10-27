(ns math.main)

(defn sin [x]
  #?(:clj (Math/sin x) :cljs (js/Math.sin x)))

(defn cos [x]
  #?(:clj (Math/cos x) :cljs (js/Math.cos x)))

(defn abs [x]
  #?(:clj (Math/abs x) :cljs (js/Math.abs x)))

(defn sqrt [x]
  #?(:clj (Math/sqrt x) :cljs (js/Math.sqrt x)))

(def pi #?(:clj Math/PI :cljs js/Math.PI))

(defn ** [x] (* x x))