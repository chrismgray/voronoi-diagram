(ns voronoi-diagram.rect
  (:require [voronoi-diagram.seg :as seg]
            [voronoi-diagram.pt :as pt]))

(defn update [which rect val]
  (assoc rect which val))
(def update-x1 (partial update :x1))
(def update-x2 (partial update :x2))
(def update-x1-site (partial update :x1-site))
(def update-x2-site (partial update :x2-site))
(def update-regions (partial update :regions))

(defn new-region [& coords]
  (map #(seg/new-seg %1 %2)
       (map (fn [[x y]] (pt/new-pt x y)) coords)
       (map (fn [[x y]] (pt/new-pt x y)) (conj (vec (next coords)) (first coords)))))

(defn update-all-corners [rect site]
  (-> rect 
      (update-x1-site site)
      (update-x2-site site)
      (update-regions {site (new-region [(rect :x1) (rect :y1)]
                                        [(rect :x1) (rect :y2)]
                                        [(rect :x2) (rect :y2)]
                                        [(rect :x2) (rect :y1)])})))
(defn new-rect [x1 y1 x2 y2]
  {:x1 x1 :x2 x2 :y1 y1 :y2 y2})

(defn find-highest-bisector-lower-than [s1 s2 left-rec right-rec prev-pt]
  (let [bounding-box-x (left-rec :x2)
        possibilities (if (= (prev-pt :x) (left-rec :x1))
                        (remove nil?
                                (map #(seg/find-bisector %1 s2 %2 (get-in right-rec [:regions s2]) bounding-box-x)
                                     (keys (left-rec :regions))
                                     (vals (left-rec :regions))))
                        (remove nil?
                                (map #(seg/find-bisector s1 %1 (get-in left-rec [:regions s1]) %2 bounding-box-x)
                                     (keys (right-rec :regions))
                                     (vals (right-rec :regions)))))
        possibilities-lower-than (filter #(< (get-in % [0 :e1 :y]) (prev-pt :y)) possibilities)]
    (if (empty? possibilities-lower-than)
      (pt/new-pt (if (= (prev-pt :x) (left-rec :x1)) (left-rec :x1) (right-rec :x2)) (left-rec :y2))
      (let [highest-seg (apply max-key #(get-in % [0 :e1 :y]) (filter #(< (get-in % [0 :e1 :y]) (prev-pt :y)) possibilities))]
        (get-in highest-seg [0 :e1])))))

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
        (if (= split-line-y (get-in highest-seg [1 :e2 :y]))
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
        first-segs (first-segs left-rec right-rec)
        first-pt (get-in first-segs [0 :e1])]
    (if (not= (first-pt :y) (left-rec :y1))
      (if (< (pt/sq-dist x1-site split-line-pt) (pt/sq-dist x2-site split-line-pt))
        [nil x1-site (pt/new-pt (left-rec :x1) (left-rec :y1))]
        [x2-site nil (pt/new-pt (right-rec :x2) (right-rec :y1))])
      (if (< (pt/sq-dist x1-site split-line-pt) (pt/sq-dist x2-site split-line-pt))
        [((first first-segs) :neighbor) x1-site]
        [x2-site ((second first-segs) :neighbor)]))))

(defn next-site [r1 r2 new-pt]
  (let [new-seg (first (filter (partial seg/pt-on-seg? new-pt) (concat r1 r2)))]
    (if (nil? new-seg)
      nil
      (new-seg :neighbor))))

(defn next-sites [s1 s2 left-rec right-rec new-pt]
  (let [next-s (next-site (get-in left-rec [:regions s1])
                          (get-in right-rec [:regions s2]) new-pt)]
    (if (= (new-pt :y) (left-rec :y2))
      [nil nil]
      (if (nil? next-s)
        (if (= (new-pt :x) (left-rec :x1))
          [nil s2]
          [s1 nil])
        (if (nil? (get-in left-rec [:regions next-s]))
          [s1 next-s]
          [next-s s2])))))

(defn sites-list
  ([left-rec right-rec]
     (let [[s1 s2 first-pt] (first-sites left-rec right-rec)]
       (sites-list s1 s2 left-rec right-rec first-pt)))
  ([s1 s2 left-rec right-rec prev-pt]
     (if (and (nil? s1) (nil? s2))
       nil
       (let [r1 (get-in left-rec [:regions s1])
             r2 (get-in right-rec [:regions s2])
             bounding-box-x (left-rec :x2)
             new-pt (if (and s1 s2)
                      (get-in (seg/find-bisector s1 s2 r1 r2 bounding-box-x) [0 :e2])
                      (find-highest-bisector-lower-than s1 s2 left-rec right-rec prev-pt))
             [next-s1 next-s2] (next-sites s1 s2 left-rec right-rec new-pt)]
         (lazy-seq (cons [s1 s2] (sites-list next-s1 next-s2 left-rec right-rec new-pt)))))))

(defn insinuate-segs [segs region]
  (let [e1 ((first segs) :e1)
        e2 ((last segs) :e2)
        good-segs (->> (lazy-cat region region)
                       (drop-while (complement (partial seg/pt-on-line? e2)))
                       (take-while (complement (partial seg/pt-on-line? e1))))
        e1-seg (first (drop-while (complement (partial seg/pt-on-line? e1)) region))
        e2-seg (first good-segs)
        prev-seg (seg/new-seg (e1-seg :e1) ((first segs) :e1) (e1-seg :neighbor))
        next-seg (seg/new-seg ((last segs) :e2) (e2-seg :e2) (e2-seg :neighbor))]
    (concat (rest good-segs) (list prev-seg) segs (list next-seg))))

(defn insinuate-seg [seg region]
  (insinuate-segs (list seg) region))

(defn merge-helper [[m left-rec right-rec] [s1 s2]]
  (let [r1 (get-in left-rec [:regions s1])
        r2 (get-in right-rec [:regions s2])
        bounding-box-x (left-rec :x2)]
    (cond (nil? s1)
          [(assoc m s2 (conj (get m s2 []) nil)) [left-rec right-rec]]
          (nil? s2)
          [(assoc m s1 (vec (cons nil (get m s1 [])))) [left-rec right-rec]]
          :else
          (let [[down-bisector up-bisector] (seg/find-bisector s1 s2 r1 r2 bounding-box-x)]
           [(-> m
                (assoc s1 (vec (cons up-bisector (get m s1 []))))
                (assoc s2 (conj (get m s2 []) down-bisector))) left-rec right-rec]))))

(defn bisector-path-map-fix
  "Some of the segments in the bisector-path-map might be nil.
   We are guaranteed that at most one in a row is, so we fix the
   nil segments by looking at the next or previous segment."
  [[b-p-m left-rec right-rec] key]
  (vector
   (update-in b-p-m [key]
              #(vec
                (map (fn [prev curr next-seg]
                       (cond (not (nil? curr))
                             curr
                             (and (not (nil? prev)) (not (nil? next-seg)))
                             (seg/new-seg (prev :e2) (next-seg :e1))
                             (and (nil? prev) (not (nil? next-seg)))
                             (seg/new-seg (pt/new-pt (get-in next-seg [:e1 :x])
                                                     (if (< (get-in next-seg [:e1 :y])
                                                            (get-in next-seg [:e2 :y]))
                                                       (left-rec :y2)
                                                       (left-rec :y1)))
                                          (next-seg :e1))
                             (and (not (nil? prev)) (nil? next-seg))
                             (seg/new-seg (prev :e2) (pt/new-pt
                                                      (get-in prev [:e2 :x])
                                                      (if (< (get-in prev [:e1 :y])
                                                             (get-in prev [:e2 :y]))
                                                        (left-rec :y1)
                                                        (left-rec :y2))))
                             :else
                             (assert false)))
                     (cons nil %)       ; segs with nil in front
                     %                  ; segs
                     (conj % nil)       ; segs with nil in back
                     ))) left-rec right-rec))

(defn region-insinuator [rec [site segs]]
  (assoc-in rec [:regions site] (insinuate-segs segs (get-in rec [:regions site]))))

(defn merge-rects [left-rec right-rec]
  (let [sites-to-consider (sites-list left-rec right-rec)
        rec (new-rect (left-rec :x1) (left-rec :y1) (right-rec :x2) (right-rec :y2))
        [bisector-path-map _ _] (reduce merge-helper [{} left-rec right-rec] sites-to-consider)
        [bisector-path-map _ _] (reduce bisector-path-map-fix [bisector-path-map left-rec right-rec] (keys bisector-path-map))
        rec (assoc rec :regions (merge (left-rec :regions) (right-rec :regions)))
        rec (reduce region-insinuator rec bisector-path-map)
        first-seg (first (first-segs left-rec right-rec))
        new-x1-site (if (= (get-in [:e1 :x] first-seg) (left-rec :x1))
                      (right-rec :x1-site)
                      (left-rec :x1-site))
        new-x2-site (if (= (get-in [:e1 :x] first-seg) (right-rec :x2))
                      (left-rec :x2-site)
                      (right-rec :x2-site))]
    (assoc rec
      :x1-site new-x1-site
      :x2-site new-x2-site)))

