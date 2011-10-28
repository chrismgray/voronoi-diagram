(ns voronoi-diagram.seg
  (:require [voronoi-diagram.pt :as pt]))

(defmacro possibly-infinite
  "A macro to avoid dividing by 0.  It takes a body that is a division and returns
  the result of the division if neither of the arguments are 0 or nil. If the second
  argument is 0, then it returns nil.  If the first argument is nil, it returns nil,
  and if the second argument is nil, then it returns 0.

  This might not be the best way of doing things.  It could be easier just to do a
  try/catch to get rid of divide-by-zero."
  [body]
  (let [op (first body)
        args (rest body)]
    (case op
      /
      (let [[arg1 arg2] (vec args)]
       `(if (= 0 ~arg2)
          :infinity
          (if (= :infinity ~arg2)
            0
            (if (= :infinity ~arg1)
              :infinity
              (/ ~arg1 ~arg2)))))
      +
      `(if (some #(= :infinity %) (list ~@args))
         :infinity
         (+ ~@args))
      -
      `(if (some #(= :infinity %) (list ~@args))
         :infinity
         (- ~@args))
      *
      `(if (some #(= 0 %) (list ~@args))
         0
         (if (some #(= :infinity %) (list ~@args))
           :infinity
           (* ~@args))))))

(defn infinite? [x]
  (= x :infinity))

(defn new-seg [e1 e2 & neighbor]
  (let [slope (possibly-infinite (/ (- (e1 :y) (e2 :y)) (- (e1 :x) (e2 :x))))
        y-intercept (possibly-infinite (- (e1 :y) (possibly-infinite (* slope (e1 :x)))))]
    {:e1 e1 :e2 e2 :neighbor (first neighbor) :slope slope :y-intercept y-intercept}))

(defn all-same? [pred coll]
  (or (every? pred coll)
      (not-any? pred coll)))

(defn pt-within-seg-range? [pt seg]
  (let [pt-x (pt :x)
        pt-y (pt :y)
        x-diffs (map #(- pt-x (get-in seg [% :x])) '(:e1 :e2))
        y-diffs (map #(- pt-y (get-in seg [% :y])) '(:e1 :e2))]
    (and
     (all-same? #(> 0 %) x-diffs)
     (all-same? #(> 0 %) y-diffs))))

(defn pt-on-seg? [pt seg]
  (if (infinite? (seg :slope))
    (and (= (pt :x) (get-in seg [:e1 :x]))
         (pt-within-seg-range? pt seg))
    (and (= (pt :y) (+ (* (seg :slope) (pt :x)) (seg :y-intercept)))
         (pt-within-seg-range? pt seg))))

(defn seg-intersection [s1 s2]
  (if (= (s1 :slope) (s2 :slope))
    nil
    (if (infinite? (s1 :slope))
      (let [x (get-in s1 [:e1 :x])]
        (pt/new-pt x (+ (* (s2 :slope) x) (s2 :y-intercept))))
      (let [x (possibly-infinite (/ (- (s2 :y-intercept) (s1 :y-intercept))
                                    (- (s1 :slope) (s2 :slope))))]
        (pt/new-pt x (+ (* (s1 :slope) x) (s1 :y-intercept)))))))

(defn find-bisector
  "Takes the pts defined by sites s1 and s2 and their regions.
   Returns the bisector of s1 and s2 inside the intersection of their
   regions if we pretend that the bounding boxes don't exist."
  [s1 s2 r1 r2 bounding-box-x]
  (let [s1-x (s1 :x)
        s1-y (s1 :y)
        s2-x (s2 :x)
        s2-y (s2 :y)
        original-slope (possibly-infinite (/ (- s2-y s1-y) (- s2-x s1-x)))
        bisector-slope (possibly-infinite (- (possibly-infinite (/ 1 original-slope))))
        midpoint (pt/new-pt (/ (+ s1-x s2-x) 2) (/ (+ s1-y s2-y) 2))
        ]
    (if (= bisector-slope :infinity)
      ;; Find the segs directly above and below the midpoint
      (let [dummy-seg (new-seg midpoint (pt/new-pt (midpoint :x) (inc (midpoint :y))))
            possible-intersections (->> (concat r1 r2)
                                        (map (partial seg-intersection dummy-seg))
                                        (filter pt-on-seg?)
                                        (remove #(= bounding-box-x (% :x))))]
        (assert (= 2 (count possible-intersections)))
        possible-intersections)
      (let [y-intercept (- (midpoint :y) (* bisector-slope (midpoint :x)))
            dummy-seg (new-seg midpoint (pt/new-pt 0 y-intercept))
            possible-intersections (->> (concat r1 r2)
                                        (map (partial seg-intersection dummy-seg))
                                        (filter pt-on-seg?)
                                        (remove #(= bounding-box-x (% :x))))]
        (assert (= 2 (count possible-intersections)))
        possible-intersections))))