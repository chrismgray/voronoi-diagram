(ns voronoi-diagram.test.rect
  (:use [clojure.test])
  (:require [voronoi-diagram.rect :as rect]
            [voronoi-diagram.seg :as seg]
            [voronoi-diagram.pt :as pt]))

(deftest first-segs
  (let [left-rec (rect/new-rect -15 10 0 -10)
        right-rec (rect/new-rect 0 10 15 -10)
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -3])
        r2 (rect/new-region [-1 10] [-15 -3] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        left-rec (assoc left-rec :x1-site site1 :x2-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :x1-site site3 :x2-site site3
                         :regions {site3 r3})]
    (is (= 2 (count
              (rect/first-segs left-rec right-rec))))))

(deftest first-sites
  (let [left-rec (rect/new-rect -15 10 0 -10)
        right-rec (rect/new-rect 0 10 15 -10)
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -3])
        r2 (rect/new-region [-1 10] [-15 -3] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        left-rec (assoc left-rec :x1-site site1 :x2-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :x1-site site3 :x2-site site3
                         :regions {site3 r3})]
    (is (= [site1 site3]
           (rect/first-sites left-rec right-rec)))))

(deftest sites-list
  (let [left-rec (rect/new-rect -15 10 0 -10)
        right-rec (rect/new-rect 0 10 15 -10)
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -3])
        r2 (rect/new-region [-1 10] [-15 -3] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        neighbor-seg (seg/new-seg (pt/new-pt -15 -3) (pt/new-pt -1 10) site2)
        r1 (assoc-in (vec r1) [2] neighbor-seg)
        left-rec (assoc left-rec :x1-site site1 :x2-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :x1-site site3 :x2-site site3
                         :regions {site3 r3})]
    (is (= [[site1 site3] [site2 site3]]
           (vec (rect/sites-list left-rec right-rec))))))