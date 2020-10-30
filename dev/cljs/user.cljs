(ns cljs.user
  (:require [portal.web :as p]))

(p/tap)

(comment
  (p/open)
  (def portal (p/open))
  (tap> "bls"))