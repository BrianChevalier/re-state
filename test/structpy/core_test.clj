(ns structpy.core-test
  (:require [clojure.test :refer :all]
            [structpy.core :as sp]))

(deftest a-test
  (testing "Test node creation"
    (is (sp/Node 0 0) 
        {:x 0 :y 0 :type :Node :fixity :free})))

(def elem
  (sp/Element
   (sp/Node 0 0 :fixity :pin)
   (sp/Node 1 0 :fixity :pin)
   (sp/Material 29000)
   (sp/Circle 1)))

(deftest len-test
  (testing ""
    (is (= 1.0 (sp/length elem)))))

