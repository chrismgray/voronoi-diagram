(ns voronoi-diagram.rect
  (:require [voronoi-diagram.seg :as seg]
            [voronoi-diagram.pt :as pt]))

(defn update [which rect val]
  (assoc rect which val))
(def update-top-left (partial update :top-left))
(def update-bottom-right (partial update :bottom-right))
(def update-top-left-site (partial update :top-left-site))
(def update-top-right-site (partial update :top-right-site))
(def update-regions (partial update :regions))

(defn new-region [& coords]
  (map #(seg/new-seg %1 %2)
       (map (fn [[x y]] (pt/new-pt x y)) coords)
       (map (fn [[x y]] (pt/new-pt x y)) (conj (vec (next coords)) (first coords)))))

(defn get-att [att rect]
  (get-in rect att))

(def left-x (partial get-att [:top-left :x]))
(def right-x (partial get-att [:bottom-right :x]))
(def top-y (partial get-att [:top-left :y]))
(def bottom-y (partial get-att [:bottom-right :y]))
(def top-left-site (partial get-att [:top-left-site]))
(def top-right-site (partial get-att [:top-right-site]))

(defn update-all-corners [rect site]
  (-> rect 
      (update-top-left-site site)
      (update-top-right-site site)
      (update-regions {site (new-region [(left-x rect) (top-y rect)]
                                        [(left-x rect) (bottom-y rect)]
                                        [(right-x rect) (bottom-y rect)]
                                        [(right-x rect) (top-y rect)])})))

(defn new-rect [top-left bottom-right]
  {:top-left top-left :bottom-right bottom-right})

(defn find-highest-bisector-lower-than [s1 s2 left-rec right-rec prev-pt]
  (let [bounding-box-x (right-x left-rec)
        possibilities (remove nil?
                              (if (= (prev-pt :x) (left-x left-rec))
                                (map #(seg/find-bisector %1 s2 %2 (get-in right-rec [:regions s2]) bounding-box-x)
                                          (keys (left-rec :regions))
                                          (vals (left-rec :regions)))
                                (map #(seg/find-bisector s1 %1 (get-in left-rec [:regions s1]) %2 bounding-box-x)
                                     (keys (right-rec :regions))
                                     (vals (right-rec :regions)))))
        possibilities-lower-than (filter #(<= (get-in % [0 :e1 :y]) (prev-pt :y)) possibilities)]
    (if (empty? possibilities-lower-than)
      [(seg/new-seg (pt/new-pt (if (= (prev-pt :x) (left-x left-rec)) (left-rec :x1) (right-x right-rec)) (bottom-y left-rec)) (pt/new-pt 0 0))] ; second pt is a dummy
      (let [highest-seg (apply max-key #(get-in % [0 :e1 :y]) possibilities-lower-than)]
        highest-seg))))

(defn first-segs [left-rec right-rec]
  (let [x1-site (top-left-site right-rec)
        x2-site (top-right-site left-rec)
        _ (prn (get-in left-rec [:regions x2-site]))
        split-line-x (left-x right-rec)
        split-line-y (top-y right-rec)
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
                (filter #(= (left-x left-rec) (get-in % [0 :e1 :x])))
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
  (let [s2 (right-rec :top-left-site)
        s1 (left-rec :top-right-site)
        split-line-x (left-x right-rec)
        split-line-y (top-y right-rec)
        split-line-pt (pt/new-pt split-line-x split-line-y)
        first-segs (first-segs left-rec right-rec)
        first-pt (get-in first-segs [0 :e1])]
    (if (not= (first-pt :y) (top-y left-rec))
      (if (< (pt/sq-dist s2 split-line-pt) (pt/sq-dist s1 split-line-pt))
        [nil s2 (pt/new-pt (left-x left-rec) (top-y left-rec))]
        [s1 nil (pt/new-pt (right-rec :x2) (top-y right-rec))])
      (if (< (pt/sq-dist s2 split-line-pt) (pt/sq-dist s1 split-line-pt))
        [((first first-segs) :neighbor) s2 first-pt]
        [s1 ((second first-segs) :neighbor) first-pt]))))

(defn next-site [s1 s2 left-rec right-rec new-pt]
  (let [r1 (get-in left-rec [:regions s1])
        r2 (get-in right-rec [:regions s2])
        new-seg (first (filter (partial seg/pt-on-seg? new-pt) (lazy-cat r1 r2)))]
    (if (not (nil? new-seg))
      (new-seg :neighbor)
      (let [possible-seg (find-highest-bisector-lower-than s1 s2 left-rec right-rec new-pt)]
        (if (= new-pt (get-in possible-seg [0 :e1]))
          (if s1
            (get-in possible-seg [1 :neighbor])
            (get-in possible-seg [0 :neighbor]))
          nil)))))

(defn next-sites [s1 s2 left-rec right-rec new-pt]
  (let [next-s (next-site s1 s2 left-rec right-rec new-pt)]
    (if (= (new-pt :y) (bottom-y left-rec))
      [nil nil]
      (if (nil? next-s)
        (if (= (new-pt :x) (left-x left-rec))
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
             bounding-box-x (right-x left-rec)
             new-pt (if (and s1 s2)
                      (get-in (seg/find-bisector s1 s2 r1 r2 bounding-box-x) [0 :e2])
                      (get-in (find-highest-bisector-lower-than s1 s2 left-rec right-rec prev-pt) [0 :e1]))
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
        bounding-box-x (right-x left-rec)]
    (cond (nil? s1)
          [(assoc m s2 (conj (get m s2 []) nil)) left-rec right-rec]
          (nil? s2)
          [(assoc m s1 (vec (cons nil (get m s1 [])))) left-rec right-rec]
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
                                                       (bottom-y left-rec)
                                                       (top-y left-rec)))
                                          (next-seg :e1))
                             (and (not (nil? prev)) (nil? next-seg))
                             (seg/new-seg (prev :e2) (pt/new-pt
                                                      (get-in prev [:e2 :x])
                                                      (if (< (get-in prev [:e1 :y])
                                                             (get-in prev [:e2 :y]))
                                                        (top-y left-rec)
                                                        (bottom-y left-rec))))
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
        rec (new-rect (left-rec :top-left) (right-rec :bottom-right))
        [bisector-path-map _ _] (reduce merge-helper [{} left-rec right-rec] sites-to-consider)
        [bisector-path-map _ _] (reduce bisector-path-map-fix [bisector-path-map left-rec right-rec] (keys bisector-path-map))
        rec (assoc rec :regions (merge (left-rec :regions) (right-rec :regions)))
        rec (reduce region-insinuator rec bisector-path-map)
        first-seg (first (first-segs left-rec right-rec))
        new-top-left-site (if (= (get-in first-seg [:e1 :x]) (left-rec :x1))
                            (right-rec :top-left-site)
                            (left-rec :top-left-site))
        new-top-right-site (if (= (get-in first-seg [:e1 :x]) (right-x right-rec))
                             (left-rec :top-right-site)
                             (right-rec :top-right-site))]
    (assoc rec
      :top-left-site new-top-left-site
      :top-right-site new-top-right-site)))

