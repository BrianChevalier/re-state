(ns structpy.cross-sections)

;; Shape type constructors
(defn Circle
  "A circular cross section"
     [radius]
     {:type :Circle
      :radius radius})

(defn Square
  "A square cross section"
  [side]
  {:type :Square
   :side side})

(defn Generic-Section
  "A generic cross section"
  [A & {:keys [Ix] :or {Ix nil}}]
  {:Area A
   :Ix Ix
   :type :Generic-Section})

;; Compute area of each shape type
(defmulti area :type)

(defmethod area :Generic-Section [shape]
  (:Area shape))

(defmethod area :Circle 
  [circle]
  (* Math/PI (:radius circle) (:radius circle)))

(defmethod area :Square 
  [square]
  (* (:side square) (:side square)))

;; Compute moment of inertia for each shape
(defmulti Ix :type)

(defmethod Ix :Generic-Section 
  [shape]
  (:Ix shape))