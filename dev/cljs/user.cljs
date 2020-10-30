(ns cljs.user
  (:require [portal.web :as p]
            [clojure.set :refer [union]]))

(p/tap)

(def mat
  [{:A
    {:Rax {:x 1}
     :Fac {:x 1}
     :Fab {:x 0.5 :y 0.5}
     :Ray {:y 1}}

    :B
    {:Fab {:x 0.5 :y 0.5}
     :Fbc {:x 0.5 :y 0.5}
     1 {:y 1}}

    :C
    {:Fac {:x -1}
     :Fbc {:x -0.5 :y 0.5}
     :Rcy {:y 1}}}])

(defn outer-keys [mat]
  (into #{} (keys mat)))

(defn inner-keys [mat]
  (for [val (vals mat)]
    (concat (for [val2 (vals val)]
       (into #{} (keys val2))))))

(comment
  (p/open)
  (p/clear)
  (def portal (p/open))
  
  (outer-keys (first mat))
  (inner-keys (first mat))
  (first (vals (first mat)))
  (get-in (first mat) [keys])
  (tap> mat))