(ns voronoi-diagram.test.rect
  (:use [clojure.test])
  (:require [voronoi-diagram.rect :as rect]
            [voronoi-diagram.seg :as seg]
            [voronoi-diagram.pt :as pt]))

(deftest first-segs
  (let [left-rec (rect/new-rect (pt/new-pt -15 10) (pt/new-pt 0 -10))
        right-rec (rect/new-rect (pt/new-pt 0 10) (pt/new-pt 15 -10))
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -3])
        r2 (rect/new-region [-1 10] [-15 -3] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        left-rec (assoc left-rec :top-left-site site1 :top-right-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :top-left-site site3 :top-right-site site3
                         :regions {site3 r3})
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)]
    (is (= 2 (count
              (rect/first-segs left-rec right-rec))))))

(deftest first-sites
  (let [left-rec (rect/new-rect (pt/new-pt -15 10) (pt/new-pt 0 -10))
        right-rec (rect/new-rect (pt/new-pt 0 10) (pt/new-pt 15 -10))
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -3])
        r2 (rect/new-region [-1 10] [-15 -3] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        left-rec (assoc left-rec :top-left-site site1 :top-right-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :top-left-site site3 :top-right-site site3
                         :regions {site3 r3})
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)
        [first-site second-site _] (rect/first-sites left-rec right-rec)]
    (is (= [site1 site3]
           [first-site second-site]))))

(deftest sites-list
  (let [left-rec (rect/new-rect (pt/new-pt -15 10) (pt/new-pt 0 -10))
        right-rec (rect/new-rect (pt/new-pt 0 10) (pt/new-pt 15 -10))
        site1 (pt/new-pt -7 6)
        site2 (pt/new-pt -5 4)
        site3 (pt/new-pt 1 6)
        r1 (rect/new-region [-1 10] [-15 10] [-15 -8])
        r2 (rect/new-region [-1 10] [-15 -8] [-15 -10] [0 -10] [0 10])
        r3 (rect/new-region [0 10] [0 -10] [15 -10] [15 10])
        neighbor-seg (seg/new-seg (pt/new-pt -15 -8) (pt/new-pt -1 10) site2)
        r1 (assoc-in (vec r1) [2] neighbor-seg)
        left-rec (assoc left-rec :top-left-site site1 :top-right-site site2
                        :regions {site1 r1 site2 r2})
        right-rec (assoc right-rec :top-left-site site3 :top-right-site site3
                         :regions {site3 r3})
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)]
    (is (= [[site1 site3] [site2 site3]]
           (vec (rect/sites-list left-rec right-rec))))))

(deftest sites-list-extra
  (let [left-rec (rect/new-rect (pt/new-pt 16 100) (pt/new-pt 40 0))
        right-rec (rect/new-rect (pt/new-pt 40 100) (pt/new-pt 100 0))
        s1 (pt/new-pt 30 80)
        s2 (pt/new-pt 50 40)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)]
    (is (= [[s1 nil] [s1 s2] [nil s2]]
           (vec (rect/sites-list left-rec right-rec))))))

(deftest gahhhh
  (let [left-rec (rect/new-rect (pt/new-pt 16 100) (pt/new-pt 40 0))
        right-rec (rect/new-rect (pt/new-pt 40 100) (pt/new-pt 100 0))
        s1 (pt/new-pt 30 80)
        s2 (pt/new-pt 50 40)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)]
    (is (not (nil?
              (rect/merge-rects left-rec right-rec))))))

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
  (let [left-rec (rect/new-rect (pt/new-pt -12 8) (pt/new-pt -5 -8))
        right-rec (rect/new-rect (pt/new-pt -5 8) (pt/new-pt 0 -8))
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)]
    (is (= 2 (count (rect/first-segs left-rec right-rec))))))

(deftest sites-list-2
  (let [left-rec (rect/new-rect (pt/new-pt -12 8) (pt/new-pt -5 -8))
        right-rec (rect/new-rect (pt/new-pt -5 8) (pt/new-pt 0 -8))
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)
        [left-rec right-rec] (rect/extend-regions left-rec right-rec)]
    (is (= (vec (rect/sites-list left-rec right-rec))
           [[s1 s2] [nil s2]]))))

(deftest merge-rects
  (let [left-rec (rect/new-rect (pt/new-pt -12 8) (pt/new-pt -5 -8))
        right-rec (rect/new-rect (pt/new-pt -5 8) (pt/new-pt 0 -8))
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        left-rec (rect/update-all-corners left-rec s1)
        right-rec (rect/update-all-corners right-rec s2)
        merged-rec (rect/merge-rects left-rec right-rec)]
    (is (and
         (= -12 (rect/left-x merged-rec))
         (= 8 (rect/top-y merged-rec))
         (= 0 (rect/right-x merged-rec))
         (= -8 (rect/bottom-y merged-rec))
         (= s1 (rect/top-left-site merged-rec))
         (= s2 (rect/top-right-site merged-rec))))))

(defn cyclic-equal? [c1 c2]
  (let [len (count c1)]
    (some true? (map #(= c1 (take len (drop (- len %) (cycle c2)))) (range len)))))

(defn rects= [rect1 rect2]
  (and (every? #(= (rect1 %) (rect2 %)) '(:top-left :bottom-right :top-left-site :top-right-site))
       (= (set (keys (rect1 :regions))) (set (keys (rect2 :regions))))
       (every? #(apply cyclic-equal? %) (map #(list (get-in rect1 [:regions %]) (get-in rect2 [:regions %])) (keys (rect1 :regions))))))

(deftest merge-rects-2
  (let [left-rec (rect/new-rect (pt/new-pt -12 8) (pt/new-pt -5 -8))
        middle-rec (rect/new-rect (pt/new-pt -5 8) (pt/new-pt 0 -8))
        right-rec (rect/new-rect (pt/new-pt 0 8) (pt/new-pt 12 -8))
        s1 (pt/new-pt -6 5)
        s2 (pt/new-pt -4 3)
        s3 (pt/new-pt 1 5)
        left-rec (rect/update-all-corners left-rec s1)
        middle-rec (rect/update-all-corners middle-rec s2)
        right-rec (rect/update-all-corners right-rec s3)]
    (is (rects=
         (rect/merge-rects (rect/merge-rects left-rec middle-rec) right-rec)
         (rect/merge-rects left-rec (rect/merge-rects middle-rec right-rec))))))

(deftest inside
  (is (true? (rect/inside? (pt/new-pt 0 0)
                           (rect/new-rect (pt/new-pt -1 1)
                                          (pt/new-pt 1 -1))))))

(deftest outside
  (is (false? (rect/inside? (pt/new-pt 0 100)
                            (rect/new-rect (pt/new-pt -1 1)
                                           (pt/new-pt 1 -1))))))

(deftest extend-regions
  (let [s1 (pt/new-pt 2 2)
        s2 (pt/new-pt 30 80)
        s3 (pt/new-pt 50 40)
        r1 (rect/new-rect (pt/new-pt 0 100) (pt/new-pt 16 0))
        r2 (rect/new-rect (pt/new-pt 16 100) (pt/new-pt 40 0))
        r3 (rect/new-rect (pt/new-pt 40 100) (pt/new-pt 100 0))
        [r1 r2 r3] (vec (map rect/update-all-corners [r1 r2 r3] [s1 s2 s3]))
        merged-r2-r3 (rect/merge-rects r2 r3)
        proper-rects [{:regions
                       {{:x 2, :y 2}
                        (list {:e1 {:x 0, :y 100},
                               :e2 {:x 0, :y 0},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept 100}
                              {:e1 {:x 0, :y 0},
                               :e2 {:x 16, :y 0},
                               :neighbor nil,
                               :slope 0,
                               :y-intercept 0}
                              {:e1 {:x 16, :y 0},
                               :e2 {:x 16, :y 100},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept :infinity}
                              {:e1 {:x 16, :y 100},
                               :e2 {:x 0, :y 100},
                               :neighbor nil,
                               :slope 0,
                               :y-intercept 100})},
                       :top-right-site {:x 2, :y 2},
                       :top-left-site {:x 2, :y 2},
                       :top-left {:x 0, :y 100},
                       :bottom-right {:x 16, :y 0}}
                      {:top-right-site {:x 30, :y 80},
                       :top-left-site {:x 30, :y 80},
                       :regions
                       {{:x 50, :y 40}
                        (list {:e1 {:x 100, :y 0},
                               :e2 {:x 100, :y 90N},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept :infinity}
                              {:e1 {:x 100, :y 90N},
                               :e2 {:x 0, :y 40N},
                               :neighbor {:x 30, :y 80},
                               :slope 1/2,
                               :y-intercept 40N}
                              {:e1 {:x 0, :y 40N},
                               :e2 {:x 0, :y 0},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept 40N}
                              {:e1 {:x 0, :y 0},
                               :e2 {:x 100, :y 0},
                               :neighbor nil,
                               :slope 0,
                               :y-intercept 0}),
                        {:x 30, :y 80}
                        (list {:e1 {:x 100, :y 90},
                               :e2 {:x 100, :y 100},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept :infinity}
                              {:e1 {:x 100, :y 100},
                               :e2 {:x 0, :y 100},
                               :neighbor nil,
                               :slope 0,
                               :y-intercept 100}
                              {:e1 {:x 0, :y 100},
                               :e2 {:x 0, :y 40},
                               :neighbor nil,
                               :slope :infinity,
                               :y-intercept 100}
                              {:e1 {:x 0, :y 40},
                               :e2 {:x 100, :y 90},
                               :neighbor {:x 50 :y 40},
                               :slope 1/2,
                               :y-intercept 40})},
                       :top-left {:x 16, :y 100},
                       :bottom-right {:x 100, :y 0}}]]
    (is (rects= (second proper-rects) (second (rect/extend-regions r1 merged-r2-r3))))))

