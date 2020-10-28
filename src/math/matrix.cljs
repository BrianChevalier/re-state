(ns math.matrix
  (:require ;[clojure.core.matrix :as mat]
            ;[clojure.core.matrix.implementations :as imp]
   [clojure.core.matrix.protocols :as proto]
   ["mathjs" :as m]))

(def mat (m/matrix (clj->js [[1 2] [3 4]])))

(comment
  (m/size mat)
  (.subset mat (m/index 1))
  (js/alert (m/subset mat (m/index 1)))
  (js/alert "")
  (js/console.log (+ 1 2 3))
  (+ 1 2 3)
  )

(extend-type m/Matrix
  proto/PDimensionInfo
  (dimensionality [m])
  (get-shape [m])
  (is-scalar? [m] false)
  (is-vector? [m] false)
  (dimension-count
    [m dimension-number])

  proto/PIndexedAccess
  (get-1d [m row])
  (get-2d [m row column])
  (get-nd [m indexes]))

(deftype Matrix []
  proto/PDimensionInfo
  (dimensionality [m])
  (get-shape [m])
  (is-scalar? [m])
  (is-vector? [m])
  (dimension-count [m dimension-number])

  proto/PIndexedAccess
  (get-1d [m row])
  (get-2d [m row column])
  (get-nd [m indexes])

  ;; proto/PNorm
  ;; (norm [m p]
  ;;   #_(cond
  ;;     ((number? p) (m/norm m))))

  ;; proto/PSolveLinear

  )

(deftype Vector [length]
  proto/PDimensionInfo
  (dimensionality [m] 1)
  (get-shape [m] [length])
  (is-scalar? [m] false)
  (is-vector? [m] true)
  (dimension-count [m dimension-number]
    (if (zero? dimension-number)
      length
      (throw (js/Error. "Error"))))

  proto/PIndexedAccess
  (get-1d [m row])
  (get-2d [m row column])
  (get-nd [m indexes]))

(defn create-vector [length]
  (Vector. length))

(deftype Implementation []
  proto/PImplementation
  (implementation-key [m]
    :mathjs)
  (meta-info [m]
    {:doc "math.js implementation"})
  (construct-matrix [m data])
  (new-vector [m length] (create-vector length))
  (new-matrix [m rows columns])
  (new-matrix-nd [m shape])
  (supports-dimensionality? [m dimensions]))
