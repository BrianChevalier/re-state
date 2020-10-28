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
  (is-mutable? [m] true)

  proto/PIndexedSettingMutable
  (set-1d! [m row v]
    (m/subset m (m/index row) v))
  (set-2d! [m row column v]
    (m/subset m (m/index row column) v))
  (set-nd! [m indexes v]
    (m/subset m (apply m/index indexes) v))

  proto/PSolveLinear
  (solve [a b]
    (m/lusolve a b))

  proto/PSetSelection
  (set-selection [a args values])

  proto/PMatrixMultiply
  (matrix-multiply [m a]
    (m/multiply m a))
  (element-multiply [m a]
    (m/dotMultiply m a))

  proto/PMatrixAdd
  (matrix-add [m a] (m/add m a))
  (matrix-sub [m a] (m/sub m a))

  proto/PNegation
  (negate [m] (m/unaryMinus m))

  proto/PMatrixProducts
  (inner-product [m a]
    (m/dot m a))
  (outer-product [m a]
    (m/multiply m (m/transpose a)))

  proto/PMatrixDivide
  (element-divide
    ([m]
     (throw (js/Error. "Not implemented")))
    ([m a]
     (m/dotDivide m)))

  proto/PTranspose
  (transpose [m]
    (m/transpose m))

  proto/PVectorCross
  (cross-product [a b]
    (m/cross a b))
  (cross-product! [a b]
    (throw (js/Error. "Not implemented")))

  proto/PMatrixOps
  (trace [m] (m/trace m))
  (determinant [m] (m/det m))
  (inverse [m] (m/inv m))

  proto/PSpecialisedConstructors
  (identity-matrix [m dims] (m/identity dims))
  (diagonal-matrix [m diagonal-values] (m/diag diagonal-values))

  proto/PNorm
  (norm [m p])

  proto/PMathsFunctions
  (abs [m] (m/abs m))
  (acos [m] (m/acos m))
  (asin [m] (m/asin m))
  (atan [m] (m/atan m))
  (cbrt [m] (m/cbrt m))
  (ceil [m] (m/ceil m))
  (cos [m] (m/cos m))
  (cosh [m] (m/cosh m))
  (exp [m] (m/exp m))
  (floor [m] (m/floor m))
  (log [m] (m/log m))
  (log10 [m] (m/log10 m))
  (round [m] (m/round m))
  (signum [m] (m/sign m))
  (sin [m] (m/sin m))
  (sinh [m] (m/sinh m))
  (sqrt [m] (m/sqrt m))
  (tan [m] (m/tan m))
  (tanh [m] (m/tanh m))
  (to-degrees [m] (m/dotMultiply m (/ 180 js/Math.PI)))
  (to-radians [m] (m/dotMultiply m (/ js/Math.PI 180))))

(imp/register-implementation (m/zeros 2 2))
(core/set-current-implementation (m/zeros 2 2))

(def mat (core/matrix
          [[1 2]
           [3 4]
           [4 5]]))

(comment
  (-> mat)
  (core/identity-matrix 3)
  (core/sub mat)
  (tap> mat)
  (type mat)
  (core/zero-matrix 3 1)
  (core/mget (core/matrix [[[20 1]]]) 0 0 1)
  (core/shape mat)
  (core/dimensionality mat)
  (core/vec? (core/matrix [0 1 2]))
  (core/vec? mat)
  (core/dimension-count mat 0)
  (core/mset! mat 0 0 100)
  (core/mget mat 2 0)
  (core/sub (core/matrix [[1 1]]))
  (core/mmul (core/matrix [[1 2 3] [4 5 6]])
             (core/matrix [[7 8] [9 10] [11 12]]))
  (core/e* (core/matrix [[1 2] [3 4]])
           (core/matrix [[1 2] [3 4]]))
  (core/sin (core/matrix [1 1]))
  (lin/solve (core/matrix [[-2, 3], [2, 1]])
             (core/matrix [11, 9])))
