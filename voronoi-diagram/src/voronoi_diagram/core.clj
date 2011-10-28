(ns voronoi-diagram.core
  (require [voronoi-diagram.rect :as rect]))

(defn voronoi-diagram-merge [left-rec right-rec]
  (rect/merge-rects left-rec right-rec))

(defn voronoi-diagram-divide-conquer [bounding-rect sites]
  (let [num-sites (count sites)]
    (if (= num-sites 1)
      (rect/update-all-corners bounding-rect (first sites))
      (let [half-sites (/ num-sites 2)
            middle-x (/ (+ (get-in sites [half-sites :x])
                           (get-in sites [(inc half-sites) :x])) 2)
            left-rect (rect/update-x2 bounding-rect middle-x)
            right-rect (rect/update-x1 bounding-rect middle-x)
            [left-split-sites right-split-sites] (split-at half-sites sites)]
        (voronoi-diagram-merge (voronoi-diagram-divide-conquer left-rec left-split-sites)
                               (voronoi-diagram-divide-conquer right-rec right-split-sites))))))

(defn voronoi-diagram [bounding-rect & sites]
  (let [sorted-sites (sort-by :x sites)
        diagram (voronoi-diagram-divide-conquer bounding-rect sorted-sites)]
    diagram))
