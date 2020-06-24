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

(def a1 (nd/Node 0 0 :structure-type :Beam))
(def b1 (nd/Node 10 0 :structure-type :Beam))
(def elements1 (el/Element a1 b1 {:E 200e9} (XS/Generic-Section 1)))
(def beam (co/Frame [a1 b1] [elements1]))
(def load- [(ld/ElementDistributedLoad (:id elements1) 1 0 :global-y)])