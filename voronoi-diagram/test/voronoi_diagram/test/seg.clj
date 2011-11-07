(ns voronoi-diagram.test.seg
  (:use [voronoi-diagram.seg]
        [voronoi-diagram.infinity])
  (:require [voronoi-diagram.pt :as pt])
  (:use [clojure.test]))

(deftest infinite-slope
  (is (infinite? ((new-seg (pt/new-pt 0 0) (pt/new-pt 0 1)) :slope))))

(deftest all-same
  (is (true? (all-same? true? [false false false]))))

(deftest not-all-same
  (is (false? (all-same? true? [false true false]))))

(deftest not-all-different
  (is (false? (all-different? true? [false false false false]))))

(deftest pt-on-line
  (is (true? (pt-on-line? (pt/new-pt 0 0)
                         (new-seg (pt/new-pt 1 1)
                                  (pt/new-pt 2 2))))))

(deftest pt-not-on-line
  (is (false? (pt-on-line? (pt/new-pt 1 0)
                          (new-seg (pt/new-pt 1 1)
                                   (pt/new-pt 2 2))))))

(deftest pt-on-line-inf-slope
  (is (true? (pt-on-line? (pt/new-pt 0 0)
                          (new-seg (pt/new-pt 0 1)
                                   (pt/new-pt 0 2))))))

(deftest pt-not-on-line-inf-slope
  (is (false? (pt-on-line? (pt/new-pt 1 0)
                           (new-seg (pt/new-pt 0 1)
                                    (pt/new-pt 0 2))))))

(deftest pt-within-seg-range
  (is (true? (pt-within-seg-range? (pt/new-pt 0 0)
                                   (new-seg (pt/new-pt -1 -1)
                                            (pt/new-pt 1 1))))))

(deftest pt-on-seg
  (is (true? (pt-on-seg? (pt/new-pt 0 0)
                         (new-seg (pt/new-pt -1 -1) (pt/new-pt 1 1))))))

(deftest pt-on-end-of-seg
  (is (true? (pt-on-seg? (pt/new-pt 0 0)
                         (new-seg (pt/new-pt 0 0) (pt/new-pt 1 1))))))

(deftest pt-on-other-end-of-seg
  (is (true? (pt-on-seg? (pt/new-pt 1 1)
                         (new-seg (pt/new-pt 0 0) (pt/new-pt 1 1))))))

(deftest pt-on-end-horiz-seg
  (is (true? (pt-on-seg? (pt/new-pt 0 0)
                         (new-seg (pt/new-pt 0 0) (pt/new-pt 1 0))))))

(deftest pt-on-other-end-horiz-seg
  (is (true? (pt-on-seg? (pt/new-pt 1 0)
                         (new-seg (pt/new-pt 0 0) (pt/new-pt 1 0))))))

(deftest pt-not-on-horiz-seg
  (is (false? (pt-on-seg? (pt/new-pt 0 0)
                          (new-seg (pt/new-pt 1 0) (pt/new-pt 2 0))))))

(deftest pt-not-on-seg
  (is (false? (pt-on-seg? (pt/new-pt 0 0.5)
                          (new-seg (pt/new-pt -1 -1) (pt/new-pt 1 1))))))

(deftest pt-within-seg-range-inf-slope
  (is (true? (pt-within-seg-range? (pt/new-pt 0 0)
                                   (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))))))

(deftest pt-on-seg-inf-slope
  (is (true? (pt-on-seg? (pt/new-pt 0 0)
                         (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))))))

(deftest pt-not-on-seg-inf-slope
  (is (false? (pt-on-seg? (pt/new-pt 0 3)
                          (new-seg (pt/new-pt 0 0) (pt/new-pt 0 1))))))

(deftest seg-int
  (is (= (pt/new-pt 0 0)
         (seg-intersection (new-seg (pt/new-pt -1 -1) (pt/new-pt 1 1))
                           (new-seg (pt/new-pt -1 1) (pt/new-pt 1 -1))))))

(deftest seg-int-inf-slope
  (is (= (pt/new-pt 0 0)
         (seg-intersection (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))
                           (new-seg (pt/new-pt 1 0) (pt/new-pt -1 0))))))

(deftest seg-int-inf-slope-2
  (is (= (pt/new-pt 0 0)
         (seg-intersection (new-seg (pt/new-pt 1 0) (pt/new-pt -1 0))
                           (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))))))

(deftest bisector
  (let [s1 (pt/new-pt -1 -1)
        s2 (pt/new-pt 1 1)
        r1 (vec (map #(new-seg %1 %2) (map pt/new-pt [0 -3 -3 0] [2 2 -2 -2]) (map pt/new-pt [-3 -3 0 0] [2 -2 -2 2])))
        r2 (vec (map #(new-seg %1 %2) (map pt/new-pt [3 0 0 3] [2 2 -2 -2]) (map pt/new-pt [0 0 3 3] [2 -2 -2 2])))]
    (is (= (new-seg (pt/new-pt -2 2) (pt/new-pt 2 -2) s1)
           (first (find-bisector s1 s2 r1 r2 0))))))

(deftest intersection-on-seg
  (let [s1 (new-seg (pt/new-pt -1 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 -2))]
    (is (true? (intersection-on-seg? s1 s2)))))

(deftest intersection-not-on-seg
  (let [s1 (new-seg (pt/new-pt -1 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 2 -1) (pt/new-pt 2 -2))]
    (is (false? (intersection-on-seg? s1 s2)))))