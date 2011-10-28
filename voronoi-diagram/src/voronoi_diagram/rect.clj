(ns voronoi-diagram.rect
  (require [voronoi-diagram.site :as site]))

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

