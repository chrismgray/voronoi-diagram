(ns voronoi-diagram.test.pt
  (:require [voronoi-diagram.pt :as pt])
  (:use [clojure.test]))

(deftest sq-dist
  (is (= 25 (pt/sq-dist (pt/new-pt 0 0) (pt/new-pt 3 4)))))

(deftest real-dist
  (is (= 5.0 (pt/dist (pt/new-pt 0 0) (pt/new-pt 3 4)))))