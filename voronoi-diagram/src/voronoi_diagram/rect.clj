(ns voronoi-diagram.rect
  (:require [voronoi-diagram.seg :as seg]
            [voronoi-diagram.pt :as pt]))

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

(defn new-region [& coords]
  (map #(seg/new-seg %1 %2)
       (map (fn [[x y]] (pt/new-pt x y)) coords)
       (map (fn [[x y]] (pt/new-pt x y)) (conj (vec (next coords)) (first coords)))))

(defn first-segs [left-rec right-rec]
  (let [x1-site (right-rec :x1-site)
        x2-site (left-rec :x2-site)
        split-line-x (right-rec :x1)
        split-line-y (right-rec :y1)
        split-line-pt (pt/new-pt split-line-x split-line-y)]
    (if (< (pt/sq-dist x1-site split-line-pt) (pt/sq-dist x2-site split-line-pt))
      ;; x1 is closer, so it must be one of the two sites
      (let [possibilities (remove nil? (map #(seg/find-bisector %1
                                                                x1-site
                                                                %2
                                                                (get-in right-rec [:regions x1-site])
                                                                split-line-x) (keys (left-rec :regions)) (vals (left-rec :regions))))
            ;; of the possibilities, find the seg that starts highest
            highest-seg (apply max-key #(get-in % [0 :e1 :y]) possibilities)]
        ;; if the one that starts the highest is on the bounding-box
        ;; boundary, then find the one that starts the farthest to the right
        (if (= split-line-y (get-in highest-seg [0 :e1 :y]))
          (->> possibilities
               (filter #(= split-line-y (get-in % [0 :e1 :y])))
               (apply max-key #(get-in % [0 :e1 :x])))
          (->>  possibilities
                (filter #(= (left-rec :x1) (get-in % [0 :e1 :x])))
                (apply max-key #(get-in % [0 :e1 :y])))))
      ;; x2 is closer, so it must be one of the two sites
      (let [possibilities (remove nil? (map #(seg/find-bisector x2-site
                                                                %1
                                                                (get-in left-rec [:regions x2-site])
                                                                %2
                                                                split-line-x) (keys (right-rec :regions)) (vals (right-rec :regions))))
            highest-seg (apply max-key #(get-in % [1 :e2 :y]) possibilities)]
        (if (= split-line-y (get-in highest-seg [0 :e2 :y]))
          (->>  possibilities
                (filter #(= split-line-y (get-in % [1 :e2 :y])))
                (apply min-key #(get-in % [1 :e2 :x])))
          (->>  possibilities
                (filter #(= (right-rec :x2) (get-in % [1 :e2 :x])))
                (apply max-key #(get-in % [1 :e2 :y]))))))))

(defn first-sites [left-rec right-rec]
  (let [x1-site (right-rec :x1-site)
        x2-site (left-rec :x2-site)
        split-line-x (right-rec :x1)
        split-line-y (right-rec :y1)
        split-line-pt (pt/new-pt split-line-x split-line-y)
        first-segs (first-segs left-rec right-rec)]
    (if (< (pt/sq-dist x1-site split-line-pt) (pt/sq-dist x2-site split-line-pt))
      [((first first-segs) :neighbor) x1-site]
      [x2-site ((second first-segs) :neighbor)])))

(defn next-site [r1 r2 new-pt]
  (let [new-seg (first (filter (partial seg/pt-on-seg? new-pt) (concat r1 r2)))]
    (new-seg :neighbor)))

(defn next-sites [s1 s2 left-rec right-rec new-pt]
  (let [next-site (next-site (get-in left-rec [:regions s1])
                             (get-in right-rec [:regions s2]) new-pt)]
    (if (nil? (get-in left-rec [:regions next-site]))
      [s1 next-site]
      [next-site s2])))

(defn sites-list
  ([left-rec right-rec]
     (let [[s1 s2] (first-sites left-rec right-rec)]
       (sites-list s1 s2 left-rec right-rec)))
  ([s1 s2 left-rec right-rec]
     (if (or (nil? s1) (nil? s2))
       nil
       (let [r1 (get-in left-rec [:regions s1])
             r2 (get-in right-rec [:regions s2])
             bounding-box-x (left-rec :x2)
             new-pt (get-in (seg/find-bisector s1 s2 r1 r2 bounding-box-x) [0 :e2])
             [next-s1 next-s2] (next-sites s1 s2 left-rec right-rec new-pt)]
         (lazy-seq (cons [s1 s2] (sites-list next-s1 next-s2 left-rec right-rec)))))))

(defn insinuate-seg [seg region]
  (let [e1 (seg :e1)
        e2 (seg :e2)
        good-segs (->> (lazy-cat region region)
                       (drop-while (complement (partial seg/pt-on-seg? e2)))
                       (take-while (complement (partial seg/pt-on-seg? e1))))
        e1-seg (first (drop-while (complement (partial seg/pt-on-seg? e1)) region))
        e2-seg (first good-segs)
        prev-seg (seg/new-seg (e1-seg :e1) (seg :e1) (e1-seg :neighbor))
        next-seg (seg/new-seg (seg :e2) (e2-seg :e2) (e2-seg :neighbor))]
    (concat (rest good-segs) (list prev-seg seg next-seg))))

(defn merge-rects [left-rec right-rec])

