(ns structpy.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]
            [structpy.cross-sections :as XS]
            [structpy.node :as nd]
            [structpy.element :as el]
            [clojure.pprint]))

(m/set-current-implementation :vectorz)


(defn Material
  "Material with Young's Modulus, E"
  [E]
  {:E E})

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
  :nodes (vec (map-indexed (fn [index node] (assoc node :index index)) nodes))
  :elements elements
  :type :Truss})

(defn nodes-by-uuid
  [nodes]
  (into {} (map (juxt :id identity) nodes)))
 
(defn with-nodes
  "Look up and insert nodes into element from the truss"
  [elem truss]
  (assoc elem 
         :SN (get (nodes-by-uuid (:nodes truss)) (:SN-id elem)) 
         :EN (get (nodes-by-uuid (:nodes truss)) (:EN-id elem))))

(defn nDoF
  "Count the number of degrees of freedom for a structure"
  [structure]
  (let [nodes (:nodes structure)]
   (* (nd/nDoF (get nodes 0)) (count nodes))))

(defn add-kglobal
  "Take in K and element and add kglobal to K and return it."
  [K elem]
  (let [dof (el/DoF elem)]
   (m/set-selection K dof dof
                    (m/add (m/matrix (m/select K dof dof))
                           (m/matrix (el/kglobal elem))))))

(defn K
  "Assemble the structural stiffness matrix"
  [structure]
   (let [nDoF (nDoF structure)
         elements (map (fn [elem] (with-nodes elem structure)) (:elements structure))]
     (reduce add-kglobal
             (m/zero-array [nDoF nDoF])
             elements)))

(defn loading->vector 
  [structure loading]
  (let [{:keys [node-numbers]} structure]
    (->> (sort-by second node-numbers)
         (map first) ;list of pairs to list of ids
         (map loading)
         (mapcat (juxt :x :y)))))

(defn boundary-conditions
  [structure]
  (mapcat (fn [node] (nd/get-fixity node)) 
          (get structure :nodes)))

(defn free-DoF
  "Get the free degree of freedom indicies"
  [structure]
  (let [DoF (boundary-conditions structure)]
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
  (m/set-selection (boundary-conditions structure) ; boundary condition array
                   (free-DoF structure) ;idicies of the selection
                   (solve structure loading))) ;what to set the selection to




;; Testing variables
(def a (nd/Node 0 0 :fixity :pin))
(def b (nd/Node 3 4))
(def c (nd/Node 6 0 :fixity :pin))

(def nodes [a b c])

(def elements
  [(el/Element a b (Material 200e9) (XS/Generic-Section 0.01))
   (el/Element b c (Material 200e9) (XS/Generic-Section 0.01))])

(def truss (Truss nodes elements))

(def loading
  {(:id a) {:x 0 :y 0}
   (:id b) {:x 8660 :y 5000}
   (:id c) {:x 0 :y 0}})