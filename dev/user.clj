(ns user
  (:require [portal.api :as p]))

(p/tap)

(comment
  (def portal (p/open)))
