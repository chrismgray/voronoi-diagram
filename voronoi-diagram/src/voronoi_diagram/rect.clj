(ns voronoi-diagram.rect
  (:require [voronoi-diagram.seg :as seg]))

(defn update [which rect val]
  (assoc rect which val))
(def update-x1 (partial update :x1))
(def update-x2 (partial update :x2))
(def update-x1-site (partial update :x1-site))
(def update-x2-site (partial update :x2-site))

(defn update-all-corners [rect site]
  (-> rect 
      (update-x1-site site)
      (update-x2-site site)))

(defn new-rect [x1 y1 x2 y2]
  {:x1 x1 :x2 x2 :y1 y1 :y2 y2})

(defn first-sites [left-rec right-rec]
  (let [x1-site (right-rec :x1-site)
        x2-site (left-rec :x2-site)]
    [x1-site x2-site]))

(defn next-site [r1 r2 new-pt]
  (let [new-seg (filter (partial seg/pt-on-seg? new-pt) (concat r1 r2))]
    (new-seg :neighbor)))

(defn merge-rects [left-rec right-rec])

