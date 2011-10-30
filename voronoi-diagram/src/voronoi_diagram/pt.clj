(ns voronoi-diagram.pt)

(defn new-pt [x y]
  {:x x :y y})

(defn square [x]
  (* x x))

(defn sq-dist [p1 p2]
  (+ (square (- (p1 :x) (p2 :x))) (square (- (p1 :y) (p2 :y)))))

(defn dist [p1 p2]
  (Math/sqrt (sq-dist p1 p2)))