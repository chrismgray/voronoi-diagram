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
        r1 (rect/new-region [-1 10] [-15 10] [-15 -8])
        r2 (rect/new-region [-1 10] [-15 -8] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        neighbor-seg (seg/new-seg (pt/new-pt -15 -8) (pt/new-pt -1 10) site2)
        r1 (assoc-in (vec r1) [2] neighbor-seg)
        left-rec (assoc left-rec :x1-site site1 :x2-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :x1-site site3 :x2-site site3
                         :regions {site3 r3})]
    (is (= [[site1 site3] [site2 site3]]
           (vec (rect/sites-list left-rec right-rec))))))

(deftest insinuate-seg
  (let [region (rect/new-region [1 1] [0 0] [3 0] [4 1])
        seg (seg/new-seg (pt/new-pt 1 0) (pt/new-pt 2 1))]
    (is (= (rect/new-region [1 1] [0 0] [1 0] [2 1])
           (rect/insinuate-seg seg region)))))

(deftest insinuate-seg-2
  (let [region (rect/new-region [3 0] [4 1] [1 1] [0 0])
        seg (seg/new-seg (pt/new-pt 1 0) (pt/new-pt 2 1))]
    (is (= (rect/new-region [1 1] [0 0] [1 0] [2 1])
           (rect/insinuate-seg seg region)))))

(deftest insinuate-seg-3
  (let [region (rect/new-region [4 1] [1 1] [0 0] [3 0])
        seg (seg/new-seg (pt/new-pt 1 0) (pt/new-pt 2 1))]
    (is (= (rect/new-region [1 1] [0 0] [1 0] [2 1])
           (rect/insinuate-seg seg region)))))

(deftest insinuate-seg-4
  (let [region (rect/new-region [0 0] [3 0] [4 1] [1 1])
        seg (seg/new-seg (pt/new-pt 1 0) (pt/new-pt 2 1))]
    (is (= (rect/new-region [1 1] [0 0] [1 0] [2 1])
           (rect/insinuate-seg seg region)))))

(deftest insinuate-segs
  (let [region (rect/new-region [0 0] [3 0] [4 1] [1 1])
        segs [(seg/new-seg (pt/new-pt 4 0) (pt/new-pt 4.5 0.5))
              (seg/new-seg (pt/new-pt 4.5 0.5) (pt/new-pt 5 1))]]
    (is (= (rect/new-region [1 1] [0 0] [4 0] [4.5 0.5] [5 1])
           (rect/insinuate-segs segs region)))))

(deftest first-segs-2
  (let [left-rec (rect/new-rect -12 8 -5 -8)
        right-rec (rect/new-rect -5 8 0 -8)
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)]
    (is (= 2 (count (rect/first-segs left-rec right-rec))))))

(deftest sites-list-2
  (let [left-rec (rect/new-rect -12 8 -5 -8)
        right-rec (rect/new-rect -5 8 0 -8)
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)]
    (is (= (vec (rect/sites-list left-rec right-rec))
           [[s1 s2] [nil s2]]))))

(deftest merge-rects
  (let [left-rec (rect/new-rect -12 8 -5 -8)
        right-rec (rect/new-rect -5 8 0 -8)
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)
        merged-rec (rect/merge-rects left-rec right-rec)]
    (is (and
         (= -12 (merged-rec :x1))
         (= 8 (merged-rec :y1))
         (= 0 (merged-rec :x2))
         (= -8 (merged-rec :y2))
         (= s1 (merged-rec :x1-site))
         (= s2 (merged-rec :x2-site))))))