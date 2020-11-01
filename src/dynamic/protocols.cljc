(ns dynamic.protocols
  (:require [clojure.spec.alpha :as s]
            [clojure.repl]))


(defn valid-state-var? [thing]
  (if (= thing nil) false true))

(s/def ::t_0 number?)
(s/def ::dt number?)
(s/def ::t_n number?)

(s/def ::x_0 valid-state-var?)
(s/def ::v_0 valid-state-var?)
(s/def ::x_n valid-state-var?)
(s/def ::v_n valid-state-var?)
(s/def ::a_n valid-state-var?)

(s/def ::a_0 fn?)
(s/def ::residual fn?)
(s/def ::tangent fn?)
(s/def ::draw-state fn?)

(s/def ::system
       (s/keys :req-un [::t_0 ::x_0 ::v_0 ::a_0 ::dt ::residual ::tangent ::draw-state]))

(s/def ::state
       (s/keys :req-un [::t_n ::x_n ::v_n ::a_n]))

(defn valid-system? [system]
  (s/assert ::system system))

(defn valid-state? [state]
  (s/assert ::state state))

(comment
  ;;(s/)
  (s/check-asserts true)
 ;;(s/valid? residual residual)
  #_(spec-test/instrument `residual))
