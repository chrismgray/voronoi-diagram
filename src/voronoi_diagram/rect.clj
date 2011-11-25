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

(defn new-rect
  "Creates a new rectangle with top-left point and bottom-right point
   as created by voronoi-diagram.pt/new-pt"
  [top-left bottom-right]
  {:top-left top-left :bottom-right bottom-right})

(defn inside?
  "Returns true if the pt is inside the rectangle."
  [pt rec]
  (and (<= (pt :x) (right-x rec))
       (>= (pt :x) (left-x rec))
       (<= (pt :y) (top-y rec))
       (>= (pt :y) (bottom-y rec))))

(defn find-highest-bisector-lower-than
  "Finds the bisector of one of the two sites s1 or s2 (the one that is not nil)
   and a site in either left-rec or right-rec depending on which of s1 or s2 is nil.
   The bisector returned is the bisector whose top endpoint is the highest, but
   lower than prev-pt."
  [s1 s2 left-rec right-rec prev-pt]
  (let [bounding-box-x (right-x left-rec)
        find-bis (fn [x y] (seg/find-bisector x y (get-in left-rec [:regions x]) (get-in right-rec [:regions y])))
        possibilities (remove nil?
                              (if (nil? s1)
                                (map find-bis (keys (:regions left-rec)) (repeat s2))
                                (map find-bis (repeat s1) (keys (:regions right-rec)))))
        possibilities-lower-than (filter #(<= (get-in % [0 :e1 :y]) (prev-pt :y)) possibilities)]
    (if (empty? possibilities-lower-than)
      [(seg/new-seg (pt/new-pt (if (= (prev-pt :x) (left-x left-rec))
                                 (left-x left-rec)
                                 (right-x right-rec)) (bottom-y left-rec)) (pt/new-pt 0 0))] ; second pt is a dummy
      (let [highest-seg (apply max-key #(get-in % [0 :e1 :y]) possibilities-lower-than)]
        highest-seg))))

(defn first-segs
  "Finds the first (pair of) segment(s) in the bisector path.
   We know that this is either defined by the site whose region
   contains the top-right corner of the left-rec or the site whose
   region contains the top-left corner of the right-rec."
  [left-rec right-rec]
  (let [s1 (left-rec :top-right-site)
        s2 (right-rec :top-left-site)
        split-line-pt (pt/new-pt (left-x right-rec) (top-y right-rec))
        dist (partial pt/sq-dist split-line-pt)
        find-bis (fn [x y] (seg/find-bisector x y (get-in left-rec [:regions x]) (get-in right-rec [:regions y])))
        possibilities (remove nil? (if (< (dist s1) (dist s2))
                                     (map find-bis (repeat s1) (keys (:regions right-rec)))
                                     (map find-bis (keys (:regions left-rec)) (repeat s2))))
        highest-seg (apply max-key #(get-in % [0 :e1 :y]) possibilities)]
    (if (= (get-in highest-seg [0 :e1 :y]) (:y split-line-pt))
      (->> possibilities
           (filter #(= (get-in % [0 :e1 :y]) (:y split-line-pt)))
           (apply min-key #(dist (get-in % [0 :e1]))))
      highest-seg)))

(defn first-sites
  "Finds the first pair of sites in the bisector path."
  [left-rec right-rec]
  (let [s2 (right-rec :top-left-site)
        s1 (left-rec :top-right-site)
        split-line-x (left-x right-rec)
        split-line-y (top-y right-rec)
        split-line-pt (pt/new-pt split-line-x split-line-y)
        dist (partial pt/sq-dist split-line-pt)
        first-segs (first-segs left-rec right-rec)
        first-pt (get-in first-segs [0 :e1])]
    (if (not= (first-pt :y) (top-y left-rec))
      (if (< (dist s2) (dist s1))
        [nil s2 (pt/new-pt (left-x left-rec) (top-y left-rec))]
        [s1 nil (pt/new-pt (right-rec :x2) (top-y right-rec))])
      (if (< (dist s2) (dist s1))
        [((first first-segs) :neighbor) s2 first-pt]
        [s1 ((second first-segs) :neighbor) first-pt]))))

(defn next-site
  "Finds the next site to consider in the bisector-path, given that
   the previous two sites are s1 and s2.  Returns nil if the bisector
   path next runs along the left or right edge of the bounding
   rectangles."
  [s1 s2 left-rec right-rec new-pt]
  (let [r1 (get-in left-rec [:regions s1])
        r2 (get-in right-rec [:regions s2])
        new-seg (first (filter (partial seg/pt-on-seg? new-pt) (lazy-cat r1 r2)))]
    (if (and (not (nil? s1)) (not (nil? s2)))
      (new-seg :neighbor)
      (let [possible-seg (find-highest-bisector-lower-than s1 s2 left-rec right-rec new-pt)]
        (if (= new-pt (get-in possible-seg [0 :e1]))
          (if s1
            (get-in possible-seg [1 :neighbor])
            (get-in possible-seg [0 :neighbor]))
          nil)))))

(defn next-sites
  "Finds the next pair of sites in the bisector-path, given that s1 and s2
  are the previous two sites.  If both sites returned are nil, then the
  bisector path has been completely computed.  If only one of the sites is
  nil, then the path runs along the left or right edge of the bounding rectangles. "
  [s1 s2 left-rec right-rec new-pt]
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
  "Returns a lazy list of pairs of sites that define the bisector-path.  When
   one of the sites is nil, then the bisector-path runs along one of the left
   or right edge of the bounding rectangles."
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
                      (get-in (seg/find-bisector s1 s2 r1 r2) [0 :e2])
                      (get-in (find-highest-bisector-lower-than s1 s2 left-rec right-rec prev-pt) [0 :e1]))
             [next-s1 next-s2] (next-sites s1 s2 left-rec right-rec new-pt)]
         (lazy-seq (cons [s1 s2] (sites-list next-s1 next-s2 left-rec right-rec new-pt)))))))

(defn insinuate-segs
  "Puts the segments into the region.  May extend some of the edges in the
   region so that they meet the new segments."
  [segs region]
  (let [e1 ((first segs) :e1)
        e2 ((last segs) :e2)
        good-segs (->> (lazy-cat region region)
                       (drop-while (complement (partial seg/pt-on-line? e2)))
                       (take-while (complement (partial seg/pt-on-line? e1))))
        e1-seg (first (drop-while (complement (partial seg/pt-on-line? e1)) region))
        e2-seg (first good-segs)
        prev-seg (seg/new-seg (e1-seg :e1) ((first segs) :e1) (e1-seg :neighbor))
        next-seg (seg/new-seg ((last segs) :e2) (e2-seg :e2) (e2-seg :neighbor))]
    (remove #(= (% :e1) (% :e2)) (concat (rest good-segs) (list prev-seg) segs (list next-seg)))))

(defn insinuate-seg
  "Puts a single segment into the region."
  [seg region]
  (insinuate-segs (list seg) region))

(defn merge-helper
  "Given a map and a pair of sites, associates the sites with the part of the
   actual bisector-path (that is, the segments in the bisector-path) that is
   between the two sites, as well as any previous parts of the bisector-path
   that have been computed."
  [[m left-rec right-rec] [s1 s2]]
  (let [r1 (get-in left-rec [:regions s1])
        r2 (get-in right-rec [:regions s2])
        bounding-box-x (right-x left-rec)]
    (cond (nil? s1)
          (if (nil? (last (m s2))) ; don't put multiple nils in a row
            [m left-rec right-rec]
            [(assoc m s2 (conj (get m s2 []) nil)) left-rec right-rec])
          (nil? s2)
          (if (nil? (first (m s1))) ; don't put multiple nils in a row
            [m left-rec right-rec]
            [(assoc m s1 (vec (cons nil (get m s1 [])))) left-rec right-rec])
          :else
          (let [[down-bisector up-bisector] (seg/find-bisector s1 s2 r1 r2)]
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
                             (assert (not (every? nil? [curr prev next-seg])) (str %))))
                     ;; segs with nil in front; not hitting end is okay
                     (cons nil %)
                     ;; segs 
                     %
                     ;; segs with nil in back
                     (conj (vec (rest %)) nil)))) left-rec right-rec))

(defn region-insinuator [rec [site segs]]
  (assoc-in rec [:regions site] (insinuate-segs segs (get-in rec [:regions site]))))

(defn extend-regions [left-rec right-rec]
  (let [split-line-x (left-x right-rec)
        left-bound (seg/new-seg (pt/new-pt (left-x left-rec) (top-y left-rec))
                                (pt/new-pt (left-x left-rec) (bottom-y left-rec)))
        top-left-bound (seg/new-seg (pt/new-pt (right-x left-rec) (top-y left-rec))
                                    (pt/new-pt (left-x left-rec) (top-y left-rec)))
        bottom-left-bound (seg/new-seg (pt/new-pt (left-x left-rec) (bottom-y left-rec))
                                    (pt/new-pt (right-x left-rec) (bottom-y left-rec)))
        right-bound (seg/new-seg (pt/new-pt (right-x right-rec) (top-y right-rec))
                                 (pt/new-pt (right-x right-rec) (bottom-y right-rec)))
        top-right-bound (seg/new-seg (pt/new-pt (right-x right-rec) (top-y right-rec))
                                     (pt/new-pt (left-x right-rec) (top-y right-rec)))
        bottom-right-bound (seg/new-seg (pt/new-pt (left-x right-rec) (bottom-y right-rec))
                                        (pt/new-pt (right-x right-rec) (bottom-y right-rec)))
        helper-fn (fn [top-bound mid-bound bot-bound other-rec region]
                    (let [on-split-line (->> region
                                             (filter #(or (= split-line-x (get-in % [:e1 :x]))
                                                          (= split-line-x (get-in % [:e2 :x]))))
                                             (remove #(and (= split-line-x (get-in % [:e1 :x]))
                                                           (= split-line-x (get-in % [:e2 :x]))))
                                             (vec))
                          ;; reverse segs if first one is pointing away from the split line
                          on-split-line (if (= (get-in on-split-line [0 :e1 :x]) split-line-x) (reverse on-split-line) on-split-line)
                          possible-intersection (if (empty? on-split-line) nil (apply seg/intersection on-split-line))]
                      (cond
                       (empty? on-split-line)
                       region
                       (and possible-intersection (inside? possible-intersection other-rec))
                       (insinuate-segs [(seg/new-seg (get-in on-split-line [0 :e2]) possible-intersection (get-in on-split-line [0 :neighbor]))
                                        (seg/new-seg possible-intersection (get-in on-split-line [1 :e1]) (get-in on-split-line [1 :neighbor]))]
                                       region)
                       (and (seg/intersection-on-seg? (first on-split-line) bot-bound)
                            (seg/intersection-on-seg? (second on-split-line) bot-bound))
                       (insinuate-seg (seg/new-seg (seg/intersection (first on-split-line) bot-bound)
                                                   (seg/intersection (second on-split-line) bot-bound)) region)
                       (seg/intersection-on-seg? (first on-split-line) mid-bound)
                       (cond
                        (seg/intersection-on-seg? (second on-split-line) mid-bound)
                        (insinuate-seg (seg/new-seg (seg/intersection (first on-split-line) mid-bound)
                                                    (seg/intersection (second on-split-line) mid-bound)) region)
                        (seg/intersection-on-seg? (second on-split-line) bot-bound)
                        (insinuate-segs [(seg/new-seg (seg/intersection (first on-split-line) mid-bound)
                                                      (mid-bound :e2))
                                         (seg/new-seg (mid-bound :e2)
                                                      (seg/intersection (second on-split-line) bot-bound))] region)
                        :else
                        (assert false))
                       (seg/intersection-on-seg? (first on-split-line) top-bound)
                       (cond
                        (seg/intersection-on-seg? (second on-split-line) top-bound)
                        (insinuate-seg (seg/new-seg (seg/intersection (first on-split-line) top-bound)
                                                    (seg/intersection (second on-split-line) top-bound)) region)
                        (seg/intersection-on-seg? (second on-split-line) mid-bound)
                        (insinuate-segs [(seg/new-seg (seg/intersection (first on-split-line) top-bound)
                                                      (top-bound :e2))
                                         (seg/new-seg (top-bound :e2)
                                                      (seg/intersection (second on-split-line) mid-bound))] region)
                        (seg/intersection-on-seg? (second on-split-line) bot-bound)
                        (insinuate-segs [(seg/new-seg (seg/intersection (first on-split-line) top-bound)
                                                      (top-bound :e2))
                                         mid-bound
                                         (seg/new-seg (bot-bound :e1)
                                                      (seg/intersection (second on-split-line) bot-bound))])
                        :else
                        (assert false)))))
        new-left-rec (reduce (fn [m k]
                               (update-in m [:regions k]
                                          (partial helper-fn top-right-bound right-bound bottom-right-bound right-rec)))
                             left-rec (keys (left-rec :regions)))
        new-right-rec (reduce (fn [m k]
                                (update-in m [:regions k]
                                           (partial helper-fn top-left-bound left-bound bottom-left-bound left-rec)))
                             right-rec (keys (right-rec :regions)))]
    [new-left-rec new-right-rec]))

(defn merge-rects [left-rec right-rec]
  (let [[left-rec right-rec] (extend-regions left-rec right-rec)
        sites-to-consider (sites-list left-rec right-rec)
        rec (new-rect (left-rec :top-left) (right-rec :bottom-right))
        [bisector-path-map _ _] (reduce merge-helper [{} left-rec right-rec] sites-to-consider)
        [bisector-path-map _ _] (reduce bisector-path-map-fix [bisector-path-map left-rec right-rec] (keys bisector-path-map))
        rec (assoc rec :regions (merge (left-rec :regions) (right-rec :regions)))
        _ (prn sites-to-consider)
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

