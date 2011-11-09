(ns voronoi-diagram.test.core
  (:use [voronoi-diagram.core] :reload)
  (:use [clojure.test])
  (:require [voronoi-diagram.rect :as rect]
            [voronoi-diagram.pt :as pt]))

(defn cyclic-equal? [c1 c2]
  (let [len (count c1)]
    (some true? (map #(= c1 (take len (drop (- len %) (cycle c2)))) (range len)))))

(defn rects= [rect1 rect2]
  (and (every? #(= (rect1 %) (rect2 %)) '(:top-left :bottom-right :top-left-site :top-right-site))
       (= (set (keys (rect1 :regions))) (set (keys (rect2 :regions))))
       (every? #(apply cyclic-equal? %) (map #(list (get-in rect1 [:regions %]) (get-in rect2 [:regions %])) (keys (rect1 :regions))))))

(deftest diagram
  (is (rects= (voronoi-diagram (rect/new-rect (pt/new-pt 0 100) (pt/new-pt 100 0)) (pt/new-pt 2 2) (pt/new-pt 50 40) (pt/new-pt 30 80))
              (voronoi-diagram (rect/new-rect (pt/new-pt 0 100) (pt/new-pt 100 0)) (pt/new-pt 30 80) (pt/new-pt 2 2) (pt/new-pt 50 40)))))

(deftest diagram-2
  (let [rec (rect/new-rect (pt/new-pt 0 368) (pt/new-pt 1274 0))
        s1 (pt/new-pt 17 14)
        s2 (pt/new-pt 57 66)
        s3 (pt/new-pt 117 34)]
    (is (not (nil? (voronoi-diagram rec s1 s2 s3))))))