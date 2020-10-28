(ns math.matrix
  (:require
   [clojure.core.protocols :refer [Datafiable]]
   [clojure.core.matrix.implementations :as imp]
   [clojure.core.matrix.protocols :as proto]
   ["mathjs" :as m]
   [clojure.core.matrix :as core]
   [clojure.core.matrix.linear :as lin]))

(extend-type m/Matrix
  Datafiable
  (datafy [m] (.valueOf m))

  proto/PImplementation
  (implementation-key [m]
    :mathjs)
  (meta-info [m]
    {:doc "math.js implementation"})
  (construct-matrix [m data]
    (m/matrix (clj->js data)))
  (new-vector [m length]
    (m/zeros length))
  (new-matrix [m rows columns]
    (m/zeros rows columns))
  (new-matrix-nd [m shape]
    (apply m/zeros shape))
  (supports-dimensionality? [m dimensions] true)

  proto/PDimensionInfo
  (dimensionality [m]
    (-> m .size .-length))
  (get-shape [m] (.size m))
  (is-scalar? [m] (-> m .size .-length zero?))
  (is-vector? [m] (-> m .size .-length (= 1)))
  (dimension-count
    [m dimension-number]
    (let [number (aget (.size m) dimension-number)]
      (if (number? number)
        number
        (throw (js/Error. "Array does not have specified dimension")))))

  proto/PIndexedAccess
  (get-1d [m row]
    (m/row m row))
  (get-2d [m row column]
    (-> m (m/row row) (m/column column)))
  (get-nd [m indexes]
    (m/subset m (apply m/index indexes)))

  proto/PIndexedSetting
  (set-1d [m row v])
  (set-2d [m row column v])
  (set-nd [m indexes v])
  (is-mutable? [m])

  proto/PSolveLinear
  (solve [a b]
    (m/lusolve a b)))

(imp/register-implementation (m/zeros 2 2))
(core/set-current-implementation (m/zeros 2 2))

(def mat (core/matrix
          [[1 2]
           [3 4]
           [4 5]]))

(comment
  (-> mat)
  (tap> mat)
  (type mat)
  (core/mget (core/matrix [[[20 1]]]) 0 0 1)
  (core/shape mat)
  (core/dimensionality mat)
  (core/vec? (core/matrix [0 1 2]))
  (core/vec? mat)
  (core/dimension-count mat 0)
  (core/mget mat 2 0)
  (lin/solve (core/matrix [[-2, 3], [2, 1]])
             (core/matrix [11, 9])))
