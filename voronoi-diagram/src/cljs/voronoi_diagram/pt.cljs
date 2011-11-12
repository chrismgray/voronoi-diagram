(ns voronoi-diagram.pt
  (:use [voronoi-diagram.rationals :only [+ - * < >]]))

(defn new-pt [x y]
  {:x x :y y})

(defn square [x]
  (* x x))

(defn sq-dist [p1 p2]
  (+ (square (- (p1 :x) (p2 :x))) (square (- (p1 :y) (p2 :y)))))

(defn dist [p1 p2]
  (Math/sqrt (sq-dist p1 p2)))

(defn left-turn?
  "Returns true if the two directed segments p1p2 and p2p3
   make a left turn."
  [p1 p2 p3]
  (> (* (- (p2 :x) (p1 :x)) (- (p3 :y) (p1 :y)))
     (* (- (p3 :x) (p1 :x)) (- (p2 :y) (p1 :y)))))