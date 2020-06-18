(ns structpy.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]
            [structpy.cross-sections :as XS]))

(m/set-current-implementation :vectorz)


(defn Material
  "Material with Young's Modulus, E"
  [E]
  {:E E})

;; NODE
(defn Node
  "Create a new Node map"
  ([x y & {:keys [z fixity] 
           :or {z nil fixity :free}}]
   {:type :Node
    :dimm (if (nil? z) :2D :3D)
    :x x
    :y y
    :z z
    :fixity fixity
    :id (gensym)}))

(def DoF-names
  "The DoF naming for various structure types"
  { :2D {:Truss [:x :y] 
        :Frame [:x :y :z]} 
  :3D {:Truss [:x :y :θz] 
       :Frame [:x :y :z :θz :θy :θx]}})

{:2D 
 {:Truss 
  {:free [1 1]
   :pin [0 0]}
  :Frame {:free [1 1 1]}}
 :3D {:Truss
      {:free [1 1 1]
       :pin [0 0]}}}

(defn fixities
  "Get fixity conditions for node based on node type and fixity"
  [node-type fixity]
  (-> {:Node ;x, y
       {:free [1 1]
        :pin [0 0]
        :xroller [1 0]
        :yroller [0 1]}
       :NodeSpace ; x, y z
       {:free [1 1 1]
        :pin [0 0 0]}}
      node-type
      fixity))

(defn ndof-per-node
  "Look up number of degrees of freedom per node"
  [node]
  (->
   node
   :type
   {:Node 2
    :Node-Space-Truss 3
    :Node-Planar-Frame 3
    :Node-Space-Frame 6}))

; I want this to be just a function of a node
(defn node-DoF
  "Return the degree of freedom numbering for a node"
  [nDoFPerNode id]
  (for [i (range nDoFPerNode)]
    (+ (* nDoFPerNode id) i)))


;; ELEMENT
(defn Element
  "Create a new Element map"
  [SN EN mat xs]
  {:type :Element
   :SN nil ; (2SN.n + 0, 2SN.n + 1) (2EN.n + 0, 2EN.n + 1)
   :EN nil
   :SN-id (:id SN)
   :EN-id (:id EN)
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
  (/ (* (XS/area (get elem :xs)) 
        (get-in elem [:mat :E]))
     (m/magnitude (vect elem))))

(defn klocal
  "Truss Local element stiffness matrix"
  [elem]
  (m/mmul (stiffness elem) 
          [[1 -1] 
           [-1 1]]))

(defn transform
  "Truss element local-to-global transformation matrix"
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


;; (defn get-nodes
;;   "Look up all of the nodes from the elements"
;;   [elems]
;;   (->> elems
;;        (mapcat (juxt :SN :EN))
;;        (map (juxt :id identity))
;;        (into {})))

(defn DoF
  "Return the degree of freedom numbering for an element"
  [elem numbering]
  (let [start-number (get numbering (-> elem :SN-id))
        end-number (get numbering (-> elem :EN-id))
        nDoFPerNode 2]
    (concat
     (node-DoF nDoFPerNode start-number)
     (node-DoF nDoFPerNode end-number))))

(defn axial-force
   "Compute the element local axial force"
   [elem numbering displacements]
   (let [vec (unit-vector elem)
         [l m] [(m/mget vec 0) (m/mget vec 1)]]
     (m/mmul
      (stiffness elem)
      [[l m (- l) (- m)]]
      (m/select displacements (DoF elem numbering)))))


 (defn get-node-numbers
   [nodes]
   (->> nodes
        (map-indexed 
         (fn [index node] {(:id node) index}))
        vec
        (into {})))


;; TRUSS
(defn Truss
  [nodes elements]
  {:node-numbers (get-node-numbers nodes)
  :nodes (into {} (map (juxt :id identity) nodes))
  :elements elements
  :type :Truss})

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

(defn with-nodes
  "Look up and insert nodes into element from the truss"
  [elem truss]
  (assoc elem 
         :SN (get (:nodes truss) (:SN-id elem)) 
         :EN (get (:nodes truss) (:EN-id elem))))

(defn K
  "Create the global structural stiffness matrix"
  [truss]
  (let [nDoF (* 2 (count (:node-numbers truss)))
        numbering (:node-numbers truss)
        elements (:elements truss)]
    (reduce
     (fn [K elem]
       (let [pairs (ix (DoF elem numbering))
             [offset-i offset-j] (first pairs) ;; assume first pair is top-left element in matrix
             value (kglobal (with-nodes elem truss))]
         (assemble K pairs
                   (fn [i j v]
                     (+ v (m/mget value (- i offset-i) (- j offset-j)))))))
     (m/zero-array [nDoF nDoF])
     elements)))


(defn loading->vector 
  [structure loading]
  (let [{:keys [node-numbers]} structure]
    (->> (sort-by second node-numbers)
         (map first) ;list of pairs to list of ids
         (map loading)
         (mapcat (juxt :x :y)))))



(defn structure-BC 
  "Get the boundary conditions for a structure"
  [structure]
  (let [{:keys [node-numbers nodes]} structure]
    (->> (sort-by second node-numbers)
         (map first) ;list of pairs to list of ids
         (map nodes)
         (map :fixity)
         (mapcat (partial fixities :Node)))))

(defn free-DoF
  "Get the free degree of freedom indicies"
  [structure]
  (let [DoF (structure-BC structure)]
   (filter (fn [x] (not (zero? x)))
           (map * DoF (range 0 (count DoF))))))

(defn reduced-K
  "Reduce the structure stiffness matrix based on free DoF"
  [structure]
  (let [freeDoF (free-DoF structure)]
    (m/array (m/select (K structure) freeDoF freeDoF))))

(defn reduced-F
  "Reduce F"
  [loading freeDoF]
  (m/select loading freeDoF))

(defn solve
  "Solve structural system"
 [structure loading]
  (let [reducedF (reduced-F (loading->vector structure loading) (free-DoF structure)) 
        reducedK (reduced-K structure)]
   (lin/solve reducedK reducedF)))

(defn global-displacement
  "Get the global nodal displacement vector"
  [structure loading]
  (m/set-selection (structure-BC structure) ; boundary condition array
                   (free-DoF structure) ;idicies of the selection
                   (solve structure loading))) ;what to set the selection to




;; Testing variables
(def a (Node 0 0 :fixity :pin))
(def b (Node 3 4))
(def c (Node 6 0 :fixity :pin))

(def nodes [a b c])

(def node-numbers
  {(:id a) 0
   (:id b) 1
   (:id c) 2})

(def elements
  [(Element a b (Material 200e9) (XS/Generic-Section 0.01))
   (Element b c (Material 200e9) (XS/Generic-Section 0.01))])

(def truss (Truss nodes elements))

(def loading
  {(:id a) {:x 0 :y 0}
   (:id b) {:x 8660 :y 5000}
   (:id c) {:x 0 :y 0}})