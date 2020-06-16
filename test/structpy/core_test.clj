(ns structpy.core-test
  (:require [clojure.test :refer [deftest is]]
            [structpy.core :as s]
            [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]))

;; Known structure solution
(def a (s/Node 0 0 :fixity :pin))
(def b (s/Node 3 4))
(def c (s/Node 6 0 :fixity :pin))

(def node-numbers
  {(:id a) 0
   (:id b) 1
   (:id c) 2})

(def elements
  [(s/Element a b (s/Material 200e9) (XS/Generic-Section 0.01))
   (s/Element b c (s/Material 200e9) (XS/Generic-Section 0.01))])

(def truss (s/Truss node-numbers elements))

(def loading
  {(:id a) {:x 0 :y 0}
   (:id b) {:x 8660 :y 5000}
   (:id c) {:x 0 :y 0}})

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

