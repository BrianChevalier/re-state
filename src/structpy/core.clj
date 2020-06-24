(ns structpy.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]
            [structpy.cross-sections :as XS]
            [structpy.node :as nd]
            [structpy.element :as el]
            [clojure.pprint]
            [structpy.loading :as ld]))

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
        elements (map (fn [elem] (el/with-nodes elem structure)) (:elements structure))]
    (reduce add-kglobal
            (m/zero-array [nDoF nDoF])
            elements)))

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
  (let [reducedF (reduced-F (ld/F structure loading) (free-DoF structure))
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
  [(ld/NodalLoad (:id a) 0 0)
   (ld/NodalLoad (:id b) 8660 5000)
   (ld/NodalLoad (:id c) 0 0)])

;; {node-id 
;;   {:type :nodal-load 
;;    :x 
;;    :y 
;;    :Mz}}
;; {element-id
;;   {:load-type [:point :distributed [:constant :linear]]
;;    :direction [:global-x :local-x :global-y :local-y]
;;    :q0 :qL ;for constant or linear
;;    :P :a (distance from SN) ;for point load
;;    }}
;; :direction [global-x local-x global-y local-y]
;; :load-type [:point :distributed]
;; 