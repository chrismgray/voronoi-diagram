(ns voronoi-diagram.test.seg
  (:use [voronoi-diagram.seg]
        [voronoi-diagram.infinity])
  (:require [voronoi-diagram.pt :as pt]
            [voronoi-diagram.rect :as rect])
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
         (intersection (new-seg (pt/new-pt -1 -1) (pt/new-pt 1 1))
                           (new-seg (pt/new-pt -1 1) (pt/new-pt 1 -1))))))

(deftest seg-int-inf-slope
  (is (= (pt/new-pt 0 0)
         (intersection (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))
                           (new-seg (pt/new-pt 1 0) (pt/new-pt -1 0))))))

(deftest seg-int-inf-slope-2
  (is (= (pt/new-pt 0 0)
         (intersection (new-seg (pt/new-pt 1 0) (pt/new-pt -1 0))
                           (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 1))))))

(deftest bisector
  (let [s1 (pt/new-pt -1 -1)
        s2 (pt/new-pt 1 1)
        r1 (rect/new-region [-3 -2] [2 -2] [2 2] [-3 2])
        r2 (rect/new-region [-3 -3] [3 -3] [3 3] [-3 3])]
    (is (= (new-seg (pt/new-pt -2 2) (pt/new-pt 2 -2) s1)
           (first (find-bisector s1 s2 r1 r2))))))

(deftest intersection-on-seg
  (let [s1 (new-seg (pt/new-pt -1 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 0 -1) (pt/new-pt 0 -2))]
    (is (true? (intersection-on-seg? s1 s2)))))

(deftest intersection-not-on-seg
  (let [s1 (new-seg (pt/new-pt -1 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 2 -1) (pt/new-pt 2 -2))]
    (is (false? (intersection-on-seg? s1 s2)))))

(deftest degenerate-intersection-on-seg-1
  (let [s1 (new-seg (pt/new-pt 0 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 1 0) (pt/new-pt 1 -1))]
    (is (true? (intersection-on-seg? s1 s2)))))

(deftest degenerate-intersection-on-seg-2
  (let [s1 (new-seg (pt/new-pt 0 0) (pt/new-pt 1 0))
        s2 (new-seg (pt/new-pt 1 0) (pt/new-pt 2 0))]
    (is (false? (intersection-on-seg? s1 s2)))))

(deftest is-inside-region
  (let [region (rect/new-region [0 0]
                                [1 0]
                                [1 1]
                                [0 1])
        pt (pt/new-pt 0.5 0.5)]
    (is (true? (pt-inside-region? pt region)))))

(deftest is-outside-region
  (let [region (rect/new-region [0 0]
                                [1 0]
                                [1 1]
                                [0 1])
        pt (pt/new-pt 2 0)]
    (is (false? (pt-inside-region? pt region)))))

(deftest pt-on-region
  (let [region (rect/new-region [0 0]
                                [1 0]
                                [1 1]
                                [0 1])
        pt (pt/new-pt (/ 1 2) 0)]
    (is (true? (pt-inside-region? pt region)))))

(deftest good-bisector
  (let [r1   (list
              {:e1 {:x -1, :y 8},
               :e2 {:x -12, :y -3},
               :neighbor {:x -6, :y 5},
               :slope 1,
               :y-intercept 9}
              {:e1 {:x -12, :y -3},
               :e2 {:x -12, :y -8},
               :neighbor nil,
               :slope :infinity,
               :y-intercept :infinity}
              {:e1 {:x -12, :y -8},
               :e2 {:x 12, :y -8},
               :neighbor nil,
               :slope 0,
               :y-intercept -8}
              {:e1 {:x 12, :y -8},
               :e2 {:x 12, :y 8},
               :neighbor nil,
               :slope :infinity,
               :y-intercept :infinity}
              {:e1 {:x 12, :y 8},
               :e2 {:x -1, :y 8},
               :neighbor nil,
               :slope 0,
               :y-intercept 8})
        r2 (list
            {:e1 {:x 12, :y -8},
             :e2 {:x 12, :y 8},
             :neighbor nil,
             :slope :infinity,
             :y-intercept :infinity}
            {:e1 {:x 12, :y 8},
             :e2 {:x -12, :y 8},
             :neighbor nil,
             :slope 0,
             :y-intercept 8}
            {:e1 {:x -12, :y 8},
             :e2 {:x -12, :y -8},
             :neighbor nil,
             :slope :infinity,
             :y-intercept :infinity}
            {:e1 {:x -12, :y -8},
             :e2 {:x 12, :y -8},
             :neighbor nil,
             :slope 0,
             :y-intercept -8})
        s1 {:x -4, :y 3}
        s2 {:x 1, :y 5}]
    (is (not (nil? (find-bisector s1 s2 r1 r2))))))

(deftest new-inside-test
  (let [r1 (list
              {:e1 {:x -1, :y 8},
               :e2 {:x -12, :y -3},
               :neighbor {:x -6, :y 5},
               :slope 1,
               :y-intercept 9}
              {:e1 {:x -12, :y -3},
               :e2 {:x -12, :y -8},
               :neighbor nil,
               :slope :infinity,
               :y-intercept :infinity}
              {:e1 {:x -12, :y -8},
               :e2 {:x 12, :y -8},
               :neighbor nil,
               :slope 0,
               :y-intercept -8}
              {:e1 {:x 12, :y -8},
               :e2 {:x 12, :y 8},
               :neighbor nil,
               :slope :infinity,
               :y-intercept :infinity}
              {:e1 {:x 12, :y 8},
               :e2 {:x -1, :y 8},
               :neighbor nil,
               :slope 0,
               :y-intercept 8})
        s1 {:x -4, :y 3}
        s2 {:x 1, :y 5}
        pts (list {:x -5/2, :y 13/2} {:x 33/10, :y -8N} {:x -31/10, :y 8N} {:x 33/10, :y -8N})]
    (is (= [true true false true]
           (vec (map #(pt-inside-region? % r1) pts))))))

