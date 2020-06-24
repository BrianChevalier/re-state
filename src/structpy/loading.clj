(ns structpy.loading
  (:require [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]
            [structpy.node :as nd]
            [structpy.element :as el]
            [clojure.pprint]))

(defn NodalLoad
  "Create a new nodal force"
  ([node-id Px Py]
   (NodalLoad node-id Px Py 0))
  
  ([node-id Px Py Pz]
   {:type :Nodal-load
    :node-id node-id
    :Px (or Px 0)
    :Py (or Py 0)
    :Pz (or Pz 0)}))

(defn ElementDistributedLoad
  "w0: magnitude at start node
   wL: magnitude at end node
   dir: direction of force"
  [elem-id w0 wL dir]
  {:type :Element-load
   :elem-id elem-id
   :w0 w0
   :wL wL
   :dir dir})

(defn ElementPointLoad
  "a: distance from start node
   P: magnitude of the force
   dir: direction of force"
  [elem-id P a dir]
  {:type :Element-load
   :elem-id elem-id
   :P P
   :a a
   :dir dir})

(def order
  {:2D 
   {:Truss 
    {:Node [:Px :Py]} 
    :Beam
    {:Node [:Py :Mz]
     :Element [:P0 :M0 :PL :ML]} 
    :Frame 
    {:Node [:Px :Py :Mz]
     :Element [:F0 :P0 :M0 :FL :PL :ML]}} 
   :3D 
   {:Truss 
    {:Node [:Px :Py :Pz]}}})

(defn get-order
  "Look up the force order for node or element
   (e.g. [:Px :Py :Pz])"
  [thing]
   (-> order
       (get (:dimm thing))
       (get (:structure-type thing))
       (get (:type thing))))

(defn apply-order
  "Return a vector based on the order of functions in `order` if they are
   keywords, getting values from `dict`
   The default value is 0 instead of nil.
   
   => (apply-order [:c :b :a] {:a 2 :b 1})
   [0 1 2]"
  [order dict]
  (println order dict)
  ((apply juxt order) dict 0))

(defmulti resolve-load
  "Resolve the load to a vector placed at either the Node or Elem DoF"
  (fn [_ thing] (:type thing)))

(defmethod resolve-load :Node
  [load- _] load-)

(defmethod resolve-load :Element ;; distributed global-y
  ;; Returns dictionary
  [load- elem]
  (let [L (el/length elem)
        L260 (/ (* L L) 60)
        w0 (:w0 load-)
        wL (:wL load-)]
     {:P0 (* (/ L 20) (+ (* 7 w0) (* 3 wL)))
      :M0 (* L260 (+ (* 3 w0) (* 2 wL)))
      :PL (* (/ L 20) (+ (* 3 w0) (* 7 wL)))
      :ML (* L260 (+ (* 2 w0) (* 3 wL)))}))

(defn to-vector
  "Take elem or node, resolve to dict, then order."
  [load- thing]
  (apply-order (get-order thing) (resolve-load load- thing)))

(defn DoF
  "Dispatch either nodal or element DoF numbering"
  [thing]
  ((get 
    {:Node nd/DoF
     :Element el/DoF}
    (:type thing)) thing))

(defn nDoF
  "Count the number of degrees of freedom for a structure"
  [structure]
  (let [nodes (:nodes structure)]
    (* (nd/nDoF (get nodes 0)) (count nodes))))

(def process
  {:Nodal-load (fn [structure load-]
                 [load-
                  (get (nd/by-uuid (:nodes structure))
                       (get load- :node-id))])
   :Element-load (fn [structure load-]
                   [load-
                    (el/with-nodes (get (nd/by-uuid (:elements structure))
                          (get load- :elem-id)) structure)])})

(defn juxt-loads
  "juxt the load map next to the node or element"
  [structure loads]
  (map (fn [load-]
         ((get process (:type load-)) structure load-))
       loads))


(defn add-f
  [F load-thing]
  (let [[load- thing] load-thing
        dof (DoF thing)]
    (m/set-selection F dof
                     (m/add (m/matrix (m/select F dof))
                            (m/matrix (to-vector load- thing))))))
(defn F
  "Create a system loading vector based on a 
   sequence of loads for a structure"
  [structure loads]
  (reduce add-f 
          (m/zero-array [(nDoF structure)]) 
          (juxt-loads structure loads)))
