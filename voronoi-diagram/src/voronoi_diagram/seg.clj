(ns voronoi-diagram.seg
  (:require [voronoi-diagram.pt :as pt])
  (:use [voronoi-diagram.infinity]))


(defn new-seg [e1 e2 & neighbor]
  (let [slope (possibly-infinite (/ (- (e1 :y) (e2 :y)) (- (e1 :x) (e2 :x))))
        y-intercept (possibly-infinite (- (e1 :y) (possibly-infinite (* slope (e1 :x)))))]
    {:e1 e1 :e2 e2 :neighbor (first neighbor) :slope slope :y-intercept y-intercept}))

(defn all-same? [pred coll]
  (or (every? pred coll)
      (not-any? pred coll)))

(def all-different? (complement all-same?))

(defn pt-within-seg-range? [pt seg]
  (let [pt-x (pt :x)
        pt-y (pt :y)
        x-diffs (map #(- pt-x (get-in seg [% :x])) '(:e1 :e2))
        y-diffs (map #(- pt-y (get-in seg [% :y])) '(:e1 :e2))]
    ((comp not nil?)
     (or (and
          (all-different? #(> 0 %) x-diffs)
          (all-different? #(> 0 %) y-diffs))
         (and (some #(= 0 %) x-diffs)
              (all-different? #(> 0 %) y-diffs))
         (and (some #(= 0 %) y-diffs)
              (all-different? #(> 0 %) x-diffs))))))

(defn pt-on-seg? [pt seg]
  (if (infinite? (seg :slope))
    (and (= (pt :x) (get-in seg [:e1 :x]))
         (pt-within-seg-range? pt seg))
    (and (= (pt :y) (+ (* (seg :slope) (pt :x)) (seg :y-intercept)))
         (pt-within-seg-range? pt seg))))

(defn seg-intersection [s1 s2]
  (if (= (s1 :slope) (s2 :slope))
    nil
    (if (infinite? (s1 :slope))
      (let [x (get-in s1 [:e1 :x])]
        (pt/new-pt x (+ (* (s2 :slope) x) (s2 :y-intercept))))
      (if (infinite? (s2 :slope))
        (let [x (get-in s2 [:e1 :x])]
          (pt/new-pt x (+ (* (s1 :slope) x) (s1 :y-intercept))))
        (let [x (possibly-infinite (/ (- (s2 :y-intercept) (s1 :y-intercept))
                                      (- (s1 :slope) (s2 :slope))))]
          (pt/new-pt x (+ (* (s1 :slope) x) (s1 :y-intercept))))))))

(defn find-bisector
  "Takes the pts defined by sites s1 and s2 and their regions.
   Returns the bisector of s1 and s2 inside the intersection of their
   regions if we pretend that the bounding boxes don't exist.

  We assume that s1 is to the left of s2."
  [s1 s2 r1 r2 bounding-box-x]
  (let [s1-x (s1 :x)
        s1-y (s1 :y)
        s2-x (s2 :x)
        s2-y (s2 :y)
        original-slope (possibly-infinite (/ (- s2-y s1-y) (- s2-x s1-x)))
        bisector-slope (possibly-infinite (- (possibly-infinite (/ 1 original-slope))))
        midpoint (pt/new-pt (/ (+ s1-x s2-x) 2) (/ (+ s1-y s2-y) 2))
        y-intercept (if (infinite? bisector-slope) nil (- (midpoint :y) (* bisector-slope (midpoint :x))))
        dummy-seg (if (infinite? bisector-slope)
                    (new-seg midpoint (pt/new-pt (midpoint :x) (inc (midpoint :y))))
                    (new-seg midpoint (pt/new-pt (inc (midpoint :x)) (+ (* bisector-slope (inc (midpoint :x))) y-intercept))))
        possible-intersections (->> (concat r1 r2)
                                    (map (fn [x] [(seg-intersection dummy-seg x) x]))
                                    (remove #(nil? (first %)))
                                    (filter #(pt-on-seg? (first %) (second %)))
                                    (map first)
                                    (remove #(= bounding-box-x (% :x)))
                                    (set)
                                    (sort-by :y >)
                                    (vec))]
    (if (empty? possible-intersections)
      nil
      [(new-seg (first possible-intersections) (second possible-intersections) s1)
       (new-seg (second possible-intersections) (first possible-intersections) s2)])))