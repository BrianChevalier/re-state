(ns structpy.core-test
  (:require [clojure.test :refer [deftest is]]
            [structpy.core :as s]
            [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]
            [structpy.element :as el]
            [structpy.node :as nd]
            [structpy.loading :as ld]))

;; Known structure solution
(def a (nd/Node 0 0 :fixity :pin))
(def b (nd/Node 3 4))
(def c (nd/Node 6 0 :fixity :pin))


(def elements
  [(el/Element a b (s/Material 200e9) (XS/Generic-Section 0.01))
   (el/Element b c (s/Material 200e9) (XS/Generic-Section 0.01))])

(def truss (s/Truss [a b c] elements))

(def loading
  [(ld/NodalLoad (:id a) 0 0)
   (ld/NodalLoad (:id b) 8660 5000)
   (ld/NodalLoad (:id c) 0 0)])

(def expected-result
  (m/mmul 4 1e8
          [[0.36 0.48 -0.36 -0.48 0 0]
           [0.48 0.64 -0.48 -0.64 0 0]
           [-0.36 -0.48 0.72 0 -0.36 0.48]
           [-0.48 -0.64 0 1.28 0.48 -0.64]
           [0 0 -0.36 0.48 0.36 -0.48]
           [0 0 0.48 -0.64 -0.48 0.64]]))

(def disp [3.00694e-5 9.76563e-6])

(deftest structural-test-6
  (let [result (s/K truss)]
    (is (m/equals expected-result result 0.00001))
    (is (m/equals disp (s/solve truss loading) 0.00001))))

