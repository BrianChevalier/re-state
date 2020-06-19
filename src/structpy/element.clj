(ns structpy.element
  (:require [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]
            [structpy.node :as nd]))


(defn Element
  "Create a new Element map"
  [SN EN mat xs]
  {:type :Element
   :SN nil ; resolved by truss functions
   :EN nil
   :SN-id (:id SN)
   :EN-id (:id EN)
   :mat mat
   :xs xs})

(def test-elem 
  {:type :Element
  :SN {:x 0 :y 0 :type :Node :index 0 :dimm :2D :structure-type :Truss}
  :EN {:x 1 :y 0 :type :Node :index 1 :dimm :2D :structure-type :Truss}
   :xs {:type :Generic-Section :Area 1}
   :mat {:E 29000}})

(defn DoF
  "Return the degree of freedom numbering for an element"
  [elem]
  (vec (concat
        (nd/DoF (:SN elem))
        (nd/DoF (:EN elem)))))

(defn vect
  "The vector pointing along an element"
  [elem]
  (m/array
   [(- (get-in elem [:EN :x]) (get-in elem [:SN :x]))
    (- (get-in elem [:EN :y]) (get-in elem [:SN :y]))]))

(defn unit-vector
  "The unit vector along an element"
  [elem]
  (m/normalise (vect elem)))

(defn stiffness
  "Truss Element axial stiffness"
  [elem]
  (/ (* (XS/area (get elem :xs))
        (get-in elem [:mat :E]))
     (m/magnitude (vect elem))))

(defn dispatch
  "Dispatch based on element dimmension and structure type"
  [elem]
  [(-> elem :SN :dimm) (-> elem :SN :structure-type)])

(defmulti klocal
  "Compute the local element stiffness matrix"
  dispatch)

(defmethod klocal [:2D :Truss]
  ;"Truss Local element stiffness matrix"
  [elem]
  (m/mmul (stiffness elem)
          [[1 -1]
           [-1 1]]))

(defmulti transform
  "Truss element local-to-global transformation matrix"
  dispatch)

(defmethod transform [:2D :Truss]
  [elem]
  (let [vec (unit-vector elem)
        [l m] [(m/mget vec 0) (m/mget vec 1)]]
    [[l m 0 0]
     [0 0 l m]]))

(defn kglobal
  "Generic element stiffness matrix"
  [elem]
  (let [T (transform elem)]
    (-> (m/transpose T) ; thread first. this becomes first argument of next one
        (m/mmul (klocal elem))
        (m/mmul T))))

(defn axial-force
  "Compute the element local axial force"
  [elem displacements]
  (let [vec (unit-vector elem)
        [l m] [(m/mget vec 0) (m/mget vec 1)]]
    (m/mmul
     (stiffness elem)
     [[l m (- l) (- m)]]
     (m/select displacements (DoF elem)))))