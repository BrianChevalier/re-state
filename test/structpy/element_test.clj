(ns structpy.element-test
  (:require [clojure.test :refer [deftest is]]
            [structpy.cross-sections :as XS]
            [structpy.element :as el]
            [clojure.core.matrix :as m]
            [structpy.core :as s]))

(def a {:type :Node
        :x 0
        :y 0
        :dimm :2D
        :structure-type :Truss
        :fixity :free
        :index 0
        :id (gensym)})

(def b {:type :Node
        :x 10
        :y 10
        :dimm :2D
        :structure-type :Truss
        :fixity :free
        :index 1
        :id (gensym)})

(def c {:type :Node
        :x 20
        :y 0
        :dimm :2D
        :structure-type :Truss
        :fixity :free
        :index 2
        :id (gensym)})

(def elem0
  {:type :Element
   :SN a
   :EN b
   :xs {:type :Generic-Section :Area 1 :Ix 1}
   :mat {:E 29000}})

(def elem1
  {:type :Element
   :SN b
   :EN c
   :xs {:type :Generic-Section :Area 1 :Ix 1}
   :mat {:E 29000}})

(deftest sub-test
  (is (m/equals (el/vect elem0) [10 10] 1e-5))
  (is (m/equals (el/vect elem1) [10 -10] 1e-5)))

(deftest length
  (is (m/equals (el/length elem0) (m/magnitude (el/vect elem0)) 1e-5))
  (is (m/equals (el/length elem1) (m/magnitude (el/vect elem1)) 1e-5)))

(deftest DoF-numbering
  (is (el/DoF elem0) [0 1 2 3])
  (is (el/DoF elem1) [2 3 4 5]))

(deftest unit-vector
  (is (m/equals (el/unit-vector elem0) (m/normalise (el/vect elem0)) 1e-5))
  (is (m/equals (el/unit-vector elem1) (m/normalise (el/vect elem1)) 1e-5)))