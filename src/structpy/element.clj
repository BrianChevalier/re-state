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
   :xs xs
   :id (gensym)})

(def test-elem 
  {:type :Element
  :SN {:x 0 :y 0 :type :Node :index 0 :dimm :2D :structure-type :Frame}
  :EN {:x 1 :y 0 :type :Node :index 1 :dimm :2D :structure-type :Frame}
   :xs {:type :Generic-Section :Area 1 :Ix 1}
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
  (nd/sub (:EN elem) (:SN elem)))

(defn length
  "Compute the length of an element"
  [elem]
  (m/magnitude (vect elem)))

(defn unit-vector
  "The unit vector along an element"
  [elem]
  (m/div (vect elem) (length elem)))

(defn stiffness
  "Truss Element axial stiffness"
  [elem]
  (/ (* (XS/area (:xs elem))
        (-> elem :mat :E))
     (length elem)))

(defn dispatch
  "Dispatch based on element dimmension and structure type"
  [elem]
  [(-> elem :SN :dimm) (-> elem :SN :structure-type)])

(defmulti klocal
  "Compute the local element stiffness matrix"
  dispatch)

(defmethod klocal [:2D :Truss]
  [elem]
  (m/mmul (stiffness elem)
          [[1 -1]
           [-1 1]]))

(defmethod klocal [:3D :Truss]
  [elem]
  (m/mmul (stiffness elem)
          [[1 -1]
           [-1 1]]))

(defmethod klocal [:2D :Beam]
  [elem]
  (let [L (length elem)
        E (-> elem :mat :E)
        I (XS/Ix (:xs elem))
        EI (* E I)]
    (m/mul (/ EI (* L L L))
           [[12      (* 6 L)   (- 12)   (* 6 L)]
            [(* 6 L) (* 4 L L) (* -6 L) (* 2 L L)]
            [(- 12)  (* -6 L)  12       (* -6 L)]
            [(* 6 L) (* 2 L L) (* -6 L) (* 4 L L)]])))

(defmethod klocal [:2D :Frame]
  [elem]
  (let [L (length elem)
        A (XS/area (:xs elem))
        E (-> elem :mat :E)
        I (XS/Ix (:xs elem))
        AE (* A E)
        EI (* E I)
        a (/ AE L)
        b (/ EI L)
        c (/ EI (* L L))
        d (/ EI (* L L L))]
   [[a     0         0        (- a)  0           0      ]
    [0     (* 12 d)  (* 6 c)  0      (* -12 d) (* 6 c)  ]
    [0     (* 6 c)   (* 4 b)  0      (- 6 c)   (* 2 b)  ]
    [(- a) 0         0        a      0         0        ]
    [0     (* -12 d) (* -6 c) 0      (* 12 d)  (* -6 c) ]
    [0     (* 6 c)   (* 2 b)  0      (* -6 c)  (* 4 b)  ]]))


(defmulti transform
  "Truss element local-to-global transformation matrix"
  dispatch)

(defmethod transform [:2D :Truss]
  [elem]
  (let [vec (unit-vector elem)
        [l m] [(m/mget vec 0) (m/mget vec 1)]]
    [[l m 0 0]
     [0 0 l m]]))

(defmethod transform [:2D :Frame]
  [elem]
    (let [vec (unit-vector elem)
        [l m] [(m/mget vec 0) (m/mget vec 1)]]
      [[l     m 0 0     0 0]
       [(- m) l 0 0     0 0]
       [0     0 1 0     0 0]
       [0     0 0 l     m 0]
       [0     0 0 (- m) l 0]
       [0     0 0 0     0 1]]))

(defmethod transform [:3D :Truss]
  [elem]
  (let [vec (unit-vector elem)
        [l m n] [(m/mget vec 0) (m/mget vec 1) (m/mget vec 2)]]
    [[l m n  0 0 0]
     [0 0 0  l m n]]))

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

(defn with-nodes
  "Look up and insert nodes into element from the truss"
  [elem truss]
  (let [SN (get (nd/by-uuid (:nodes truss)) (:SN-id elem))
        EN (get (nd/by-uuid (:nodes truss)) (:EN-id elem))
        dimm (:dimm SN)
        structure-type (:structure-type SN)]
   (assoc elem
          :SN SN
          :EN EN
          :structure-type structure-type
          :dimm dimm)))