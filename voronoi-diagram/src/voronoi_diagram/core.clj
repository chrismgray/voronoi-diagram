(ns voronoi-diagram.core
  (require [voronoi-diagram.rect :as rect]
           [voronoi-diagram.pt :as pt]))

(defn voronoi-diagram-merge [left-rec right-rec]
  (rect/merge-rects left-rec right-rec))

(defn voronoi-diagram-divide-conquer [bounding-rect sites]
  (let [num-sites (count sites)]
    (if (= num-sites 1)
      (rect/update-all-corners bounding-rect (first sites))
      (let [half-sites (int (/ num-sites 2))
            middle-x (/ (+ (get-in sites [(dec half-sites) :x])
                           (get-in sites [half-sites :x])) 2)
            left-rect (rect/update-x2 bounding-rect middle-x)
            right-rect (rect/update-x1 bounding-rect middle-x)
            [left-split-sites right-split-sites] (split-at half-sites sites)]
        (voronoi-diagram-merge (voronoi-diagram-divide-conquer left-rect (vec left-split-sites))
                               (voronoi-diagram-divide-conquer right-rect (vec right-split-sites)))))))

(defn voronoi-diagram [bounding-rect & sites]
  (let [sorted-sites (vec (sort-by :x sites))]
    (voronoi-diagram-divide-conquer bounding-rect sorted-sites)))

