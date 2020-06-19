(ns structpy.node
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]))

(defn Node
  "Create a new Node map"
  ([x y & {:keys [z fixity structure-type]
           :or {z nil fixity :free structure-type :Truss}}]
   {:type :Node
    :dimm (if (nil? z) :2D :3D)
    :structure-type structure-type
    :x x
    :y y
    :z z
    :fixity fixity
    :id (gensym)}))

(def fixities
  "All the possible fixities for 2D and 3D trusses and frames.
   Includes the associated names and nDoF per node"
  {:2D
   {:Truss
    {:names [:x :y]
     :ndof 2
     :free [1 1]
     :pin [0 0]
     :xroller [1 0]
     :yroller [0 1]}
    :Frame
    {:names [:x :y :θz]
     :ndof 3
     :free [1 1 1]
     :pin [0 0 1]
     :xroller [1 0 1]
     :yroller [0 1 1]
     :fixed [0 0 0]}}
   :3D
   {:Truss
    {:names [:x :y :z]
     :ndof 3
     :free [1 1 1]
     :pin [0 0]}}
   :Frame
   {:names [:x :y :z :θz :θy :θx]
    :ndof 6
    :free [1 1 1 1 1 1]
    :fixed [0 0 0 0 0 0]}})

(defn get-fixity
  "Get the fixity array for a node depending on 
   dimmensions, structure type and boundary condiiton"
  [node]
  (-> fixities
      (get (:dimm node))
      (get (:structure-type node))
      (get (:fixity node))))

(defn nDoF
  "Look up number of degrees of freedom per node"
  [node]
  (-> fixities
      (get (:dimm node))
      (get (:structure-type node))
      (:ndof)))

(defn DoF
  "The degree of freedom numbering for a node"
 [node]
 (let [ndof (nDoF node)]
  (for [i (range ndof)]
    (+ (* ndof (:index node)) i))))