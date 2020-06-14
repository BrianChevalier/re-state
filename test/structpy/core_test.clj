(ns structpy.core-test
  (:require [clojure.test :refer [deftest is]]
            [structpy.core :as s]
            [clojure.core.matrix :as m]))

(def a (s/Node 0 0 :fixity :pin))
(def b (s/Node 3 4))
(def c (s/Node 6 0 :fixity :pin))

(def node-numbers
  {(:id a) 0
   (:id b) 1
   (:id c) 2})

(def elements
  [(s/Element a b (s/Material 200e9) (s/Generic-Section 0.01))
   (s/Element b c (s/Material 200e9) (s/Generic-Section 0.01))])

(def truss (s/Truss node-numbers elements))

(def expected-result
  (m/mmul 4 1e8
          [[0.36 0.48 -0.36 -0.48 0 0]
           [0.48 0.64 -0.48 -0.64 0 0]
           [-0.36 -0.48 0.72 0 -0.36 0.48]
           [-0.48 -0.64 0 1.28 0.48 -0.64]
           [0 0 -0.36 0.48 0.36 -0.48]
           [0 0 0.48 -0.64 -0.48 0.64]]))

(deftest structural-test-6
  (let [result (s/K truss)]
    (is (m/equals expected-result result 0.001))))
