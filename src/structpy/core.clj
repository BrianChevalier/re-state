(ns structpy.core
  (:require [clojure.core.matrix :as m])
  (:require [clojure.core.matrix.operators :as mx]))


(defn Material
  "Material with Young's Modulus, E"
  [E]
  {:E E})

(defn Circle
  [radius]
  {:type :Circle
   :radius radius})

(defn area
  [circle]
  (* (. Math PI)
     (Math/pow (:radius circle) 2)))

;; NODE
(defn Node
  "Create a new Node map"
  [x y & {:keys [fixity] :or {fixity :free}}]
  {:type :Node
   :x x
   :y y
   :fixity fixity
   :id (gensym)})

(def fixities
  "Fixities"
  {:free [1 1]
   :pin [0 0]
   :xroller [1 0]
   :yroller [0 1]})

(defn boundary-conditions
  "Look up boundary condition knows/unknowns"
  [node]
  (get fixities
       (:fixity node)))

;; ELEMENT
(defn Element
  "Create a new Element map"
  [SN EN mat xs]
  {:type :Element
   :SN SN; (2SN.n + 0, 2SN.n + 1) (2EN.n + 0, 2EN.n + 1)
   :EN EN ; 
   :mat mat
   :xs xs})

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
  (/ (* (area (get elem :xs)) (get-in elem [:mat :E]))
     (m/magnitude (vect elem))))

(defn klocal
  "Truss Local element stiffness matrix"
  [elem]
  (m/mmul (stiffness elem) [[1 -1] [-1 1]]))

(defn transform
  "Truss Element local-to-global transformation matrix"
  [elem]
  (let [[l m] (unit-vector elem)]
    [[l m 0 0]
     [0 0 l m]]))

(defn kglobal
  "The global element stiffness matrix"
  [elem]
  (let [T (transform elem)]
    (-> (m/transpose T) ; thread first. this becomes first argument of next one
        (m/mmul (klocal elem))
        (m/mmul T)))) 

(defn Truss 
  [nodes elements]
  {:nodes nodes
   :elements elements
   :type :Truss})

(def a (Node 0 0 :fixity :pin))
(def b (Node 1 1))
(def c (Node 2 0 :fixity :pin))

(def node-numbers
  {(:id a) 0
   (:id b) 1
   (:id c) 2})

(defn DoF
  "Return the degree of freedom numbering for an element"
  [elem numbering]
  (let [start-number (get numbering (-> elem :SN :id))
        end-number (get numbering (-> elem :EN :id))
        nDoFPerNode 2]
    (concat 
     (for [i (range nDoFPerNode)]
       (+ (* nDoFPerNode start-number) i))
     (for [i (range nDoFPerNode)]
       (+ (* nDoFPerNode end-number) i)))))

(def elements 
  [(Element a b (Material 29000) (Circle 1))
   (Element b c (Material 29000) (Circle 1))])

(def truss (Truss node-numbers elements))

(defn ix [a]
  (for [i a j a] [i j]))

(defn assemble
  "Create a matrix the same size as m, returning a matrix of the same size"
  [m pairs f]
  (reduce
   (fn [m [i j]]
     (m/mset m i j (f i j (m/mget m i j))))
   m
   pairs))

(comment
  (assemble
   (m/zero-array [10 10])
   #{[0 0] [0 1] [1 1] [1 0]}
   (fn [i j value] (+ value 200))))

(defn K 
  "Create the global structural stiffness matrix"
  [truss]
  (let [nDoF (* 2 (count (:nodes truss)))
        numbering (:node-numbers truss)
        elements (:elements truss)]
    (reduce
     (fn [K elem]
       (let [pairs (ix (DoF elements numbering))
             value (kglobal elem)]
         (assemble K pairs (fn [i j v] (+ value v)))))
     (m/zero-array [nDoF nDoF])
elements)))

(defn K2
  "Create the global structural stiffness matrix"
  [truss]
  (let [nDoF (* 2 (count (:nodes truss)))
        numbering (:node-numbers truss)
        elements (:elements truss)]
    (reduce
     (fn [K elem]
       (let [pairs (ix (DoF elements numbering))
             value (kglobal elem)]
         (mx/+= (submatrix K) (kglobal elem) )))
     (m/mutable (m/zero-array [nDoF nDoF]))
     elements)))



