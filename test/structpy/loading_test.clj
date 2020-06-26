(ns structpy.loading-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]
            [structpy.element :as el]
            [structpy.node :as nd]
            [structpy.loading :as ld]
            [structpy.core :as co]))


;; Truss loading
(def a0 (nd/Node 0 0 :fixity :pin))
(def b0 (nd/Node 3 4))
(def c0 (nd/Node 6 0 :fixity :pin))

(def nodes [a0 b0 c0])

(def elements
  [(el/Element a0 b0 {:E 200e9} (XS/Generic-Section 0.01))
   (el/Element b0 c0 {:E 200e9} (XS/Generic-Section 0.01))])

(def truss {:type :Truss
            :nodes (vec (map-indexed (fn [index node] (assoc node :index index)) nodes))
            :elements elements})

(def loading
  [(ld/NodalLoad (:id a0) 0 0)
   (ld/NodalLoad (:id b0) 8660 5000)
   (ld/NodalLoad (:id c0) 0 0)])


(deftest test-truss-loading
    (is (m/equals [0 0 8660 5000 0 0] (ld/F truss loading) 0.00001)))

;; Beam loading

(def q 0.1)
(def L 144)
(def E 29000)
(def I 555)

(def a1 (nd/Node 0 0 :fixity :fixed :structure-type :Beam))
(def b1 (nd/Node L 0 :fixity :free :structure-type :Beam))
(def elements1 (el/Element a1 b1 {:E E} (XS/Generic-Section 1 :Ix I)))
(def beam (co/Frame [a1 b1] [elements1]))
(def load- [(ld/ElementDistributedLoad (:id elements1) q q :global-y)])

(println (/ (* q L L) (* 6 E I)))
(println (/ (* q L L L L) (* 8 E I)))
;; (println (* (/ L 20) (+ (* 7 q) (* 3 q))))
;; (println (* (/ (* L L) 60) (+ (* 3 q) (* 2 q))))

(co/solve beam load-)