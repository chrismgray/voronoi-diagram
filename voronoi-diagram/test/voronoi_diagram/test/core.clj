(ns voronoi-diagram.test.core
  (:use [voronoi-diagram.core] :reload)
  (:use [clojure.test])
  (:require [voronoi-diagram.rect :as rect]
            [voronoi-diagram.pt :as pt]))

(defn cyclic-equal? [c1 c2]
  (let [len (count c1)]
    (some true? (map #(= c1 (take len (drop (- len %) (cycle c2)))) (range len)))))

(defn rects= [rect1 rect2]
  (and (every? #(= (rect1 %) (rect2 %)) '(:x1 :x2 :y1 :y2 :x1-site :x2-site))
       (= (set (keys (rect1 :regions))) (set (keys (rect2 :regions))))
       (every? #(apply cyclic-equal? %) (map #(list (get-in rect1 [:regions %]) (get-in rect2 [:regions %])) (keys (rect1 :regions))))))

(deftest diagram
  (is (rects= (voronoi-diagram (rect/new-rect 0 100 100 0) (pt/new-pt 2 2) (pt/new-pt 50 40) (pt/new-pt 30 80))
              (voronoi-diagram (rect/new-rect 0 100 100 0) (pt/new-pt 30 80) (pt/new-pt 2 2) (pt/new-pt 50 40)))))
