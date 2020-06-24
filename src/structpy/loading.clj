(ns structpy.loading
  (:require [clojure.core.matrix :as m]
            [structpy.cross-sections :as XS]
            [structpy.node :as nd]
            [structpy.element :as el]
            [clojure.pprint]))

(defn NodalLoad
  "Create a new nodal node"
  ([node-id Px Py]
   (NodalLoad node-id Px Py 0))
  
  ([node-id Px Py Pz]
   {:type :Nodal-load
    :node-id node-id
    :Px (or Px 0)
    :Py (or Py 0)
    :Pz (or Pz 0)}))

(defn ElementDistributedLoad
  ""
  [elem-id w0 wL dir]
  {:type :Element-load
   :elem-id elem-id
   :w0 w0
   :wL wL
   :dir dir})

(defn ElementPointLoad
  "a: distance from start node"
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

(defmulti resolve-load
  "Resolve the load to a vector placed at either the Node or Elem DoF"
  (fn [_ thing] (:type thing)))

(defmethod resolve-load :Node
  [load- node]
  ((apply juxt (get-order node))
   load-))

(defmethod resolve-load :Element ;; distributed global-y
  [load- elem]
  (let [L (el/length elem)
        L260 (/ (* L L) 60)
        w0 (:w0 load-)
        wL (:wL load-)]
    ((apply juxt (get-order elem))
     {:P0 (* (/ L 20) (+ (* 7 w0) (* 3 wL)))
      :M0 (* L260 (+ (* 3 w0) (* 2 wL)))
      :PL (* (/ L 20) (+ (* 3 w0) (* 7 wL)))
      :ML (* L260 (+ (* 2 w0) (* 3 wL)))}
     0)))

(defn DoF
  "Dispatch either nodal or element DoF numbering"
  [thing]
  ((get 
    {:Node nd/DoF
     :Element el/DoF}
    (:type thing)) thing))

(defn add-f
  [F load-thing]
  (let [[load- thing] load-thing
        dof (DoF thing)]
    (m/set-selection F dof
                    (m/add (m/matrix (m/select F dof))
                           (m/matrix (resolve-load load- thing))))))

(defn nDoF
  "Count the number of degrees of freedom for a structure"
  [structure]
  (let [nodes (:nodes structure)]
    (* (nd/nDoF (get nodes 0)) (count nodes))))

(def process
  {:Nodal-load (fn [structure load-]
                 [load-
                  (get (nd/by-uuid (:nodes structure))
                       (get load- :node-id))])})

(defn juxt-loads
  "juxt the load map next to the node or element"
  [structure loads]
  (map (fn [load-]
         ((get process (:type load-)) structure load-))
       loads))


(defn F
  [structure loads]
  (reduce add-f 
          (m/zero-array [(nDoF structure)]) 
          (juxt-loads structure loads)))


;; Testing variables
(def a (nd/Node 0 0 :fixity :pin))
(def b (nd/Node 3 4))
(def c (nd/Node 6 0 :fixity :pin))

(def nodes [a b c])

(def elements
  [(el/Element a b {:E 200e9} (XS/Generic-Section 0.01))
   (el/Element b c {:E 200e9} (XS/Generic-Section 0.01))])

(def truss {:type :Truss 
            :nodes (vec (map-indexed (fn [index node] (assoc node :index index)) nodes))
            :elements elements})

(def loading
  [(NodalLoad (:id a) 0 0)
   (NodalLoad (:id b) 8660 5000)
   (NodalLoad (:id c) 0 0)])


;; (defn loading->vector
;;   [structure loading]
;;   (let [{:keys [node-numbers]} structure]
;;     (->> (sort-by second node-numbers)
;;          (map first) ;list of pairs to list of ids
;;          (map loading)
;;          (mapcat (juxt :x :y)))))

;; (defn load->
;;   [structure loading]
;;   (let [dof-keys (-> nd/fixities (:dimm structure) (:type))]
;;     (->> (:nodes structure)
;;          (map :id)
;;          (map loading)
;;          (mapcat (apply juxt dof-keys)))))

