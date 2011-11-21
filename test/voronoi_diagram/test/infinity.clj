(ns voronoi-diagram.test.infinity
  (:use [voronoi-diagram.infinity])
  (:use [clojure.test]))

(deftest div-by-0
  (is (infinite? (possibly-infinite (/ (+ 1 1) (- 1 1))))))

(deftest no-div-by-0
  (is (not (infinite? (possibly-infinite (/ (+ 1 1) (+ 1 1)))))))

(deftest div-by-infinity
  (is ((partial = 0) (possibly-infinite (/ 1 (possibly-infinite (/ 1 0)))))))

(deftest multiply-by-0
  (is ((partial = 0) (possibly-infinite (* 1 2 0 (possibly-infinite (/ 1 0)) 3)))))