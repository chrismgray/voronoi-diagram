(ns voronoi-diagram.core
  (:require [voronoi-diagram.rect :as rect]
           [voronoi-diagram.pt :as pt]))

(defn voronoi-diagram-merge [left-rec right-rec]
  (rect/merge-rects left-rec right-rec))

(defn voronoi-diagram-divide-conquer [bounding-rect sites]
  (let [num-sites (count sites)]
    (if (= num-sites 1)
      (rect/update-all-corners bounding-rect (first sites))
      (let [half-sites (js/parseInt (/ num-sites 2))
            middle-x (/ (+ (get-in sites [(dec half-sites) :x])
                           (get-in sites [half-sites :x])) 2)
            left-rect (rect/update-bottom-right bounding-rect (pt/new-pt middle-x (rect/bottom-y bounding-rect)))
            right-rect (rect/update-top-left bounding-rect (pt/new-pt middle-x (rect/top-y bounding-rect)))
            [left-split-sites right-split-sites] (map vec (split-at half-sites sites))]
        (voronoi-diagram-merge (voronoi-diagram-divide-conquer left-rect left-split-sites)
                               (voronoi-diagram-divide-conquer right-rect right-split-sites))))))

(defn diagram [bounding-rect & sites]
  (let [sorted-sites (vec (sort-by :x sites))]
    (voronoi-diagram-divide-conquer bounding-rect sorted-sites)))

