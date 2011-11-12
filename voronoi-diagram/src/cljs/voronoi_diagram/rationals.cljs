(ns voronoi-diagram.rationals
  (:require [cljs.core :as core]))

;; TODO: use deftype
(defn- new-rational [num denom]
  {:rational true :num num :denom denom})

(defn- rational? [num]
  (if (map? num)
    (true? (get num :rational false))
    false))

(defn- to-float [r]
  (if (rational? r)
    (core// (r :num) (r :denom))
    r))

(defn- int->rational [i]
  (if (integer? i)
    (new-rational i 1)
    i))

(defn- infinite? [x]
  (and (symbol? x) (= x :infinity)))

(defn- should-be-rational? [& x]
  (cond
   (empty? x)
   :empty
   (every? #(integer? %) x)
   :integer
   (every? #(or (rational? %) (integer? %)) x)
   :rational
   (some infinite? x)
   :infinite
   :else
   :float))

(defn- abs [x]
  (if (> x 0)
    x
    (- x)))

(defn- gcd [x y]
  (let [[x y] [(abs x) (abs y)]
        [larger smaller] (if (> x y) [x y] [y x])]
    (if (= 0 smaller)
      larger
      (recur (- larger smaller) smaller))))

(defmulti / should-be-rational?)

(defmethod / :empty [& x] 1)

(defmethod / :integer [& x]
  (let [x1 (first x)
        xs (rest x)]
    (if (empty? xs)
      (new-rational 1 x1)
      (if (some #(= 0 %) xs)
        :infinite
        (reduce / (new-rational x1 (first xs)) (rest xs))))))

(defn- divide-rationals [x y]
  (if (integer? y)
    (* x (new-rational 1 y))
    (* x (new-rational (y :denom) (y :num)))))

(defmethod / :rational [& x]
  (let [x1 (first x)
        xs (rest x)]
    (if (empty? xs)
      (/ 1 x1)
      (reduce divide-rationals (divide-rationals x1 (first xs)) (rest xs)))))

(defmethod / :infinite [& x]
  (let [x1 (first x)
        xs (rest x)]
    (if (or (empty? xs) (infinite? x1))
      :infinite
      0)))

(defmethod / :float [& x]
  (->> x
       (map to-float)
       (apply core//)))

(defmulti * should-be-rational?)

(defmethod * :empty [& x] 1)

(defmethod * :integer [& x]
  (apply core/* x))

(defn- reduce-rational [x]
  (let [divisor (gcd (x :num) (x :denom))]
    (-> x
        (update-in [:num] #(if (core/< (x :denom) 0) (- %) %))
        (update-in [:denom] #(if (core/< % 0) (- %) %))
        (update-in [:num] #(core// % divisor))
        (update-in [:denom] #(core// % divisor)))))

(defn- multiply-rationals [x y]
  (cond
   (and (integer? x) (integer? y))
   (core/* x y)
   (and (rational? x) (rational? y))
   (reduce-rational (-> x
                        (update-in [:num] #(core/* % (y :num)))
                        (update-in [:denom] #(core/* % (y :denom)))))
   (and (rational? x) (integer? y))
   (multiply-rationals x (new-rational y 1))
   :else
   (multiply-rationals y (new-rational x 1))))

(defmethod * :rational [& x]
  (reduce multiply-rationals x))

(defmethod * :infinite [& x]
  (if (some #(= 0 %) x)
    0
    :infinity))

(defmethod * :float [& x]
  (->> x
       (map to-float)
       (apply core/*)))

(defmulti + should-be-rational?)

(defmethod + :empty [& x] 0)

(defmethod + :integer [& x]
  (apply core/+ x))

(defn- add-rationals [x y]
  (cond
   (and (rational? x) (rational? y))
   (let [denom-gcd (gcd (x :denom) (y :denom))
         lcm-multiplier-x (core// (y :denom) denom-gcd)
         lcm-multiplier-y (core// (x :denom) denom-gcd)]
     (reduce-rational
      (-> x
          (update-in [:num] #(core/+ (core/* lcm-multiplier-x %) (core/* lcm-multiplier-y (y :num))))
          (update-in [:denom] #(core/* lcm-multiplier-x %)))))
   (and (rational? x) (integer? y))
   (reduce-rational (update-in x [:num] #(core/+ % (core/* y (x :denom)))))
   :else
   (reduce-rational (update-in y [:num] #(core/+ % (core/* y (y :denom)))))))

(defmethod + :rational [& x]
  (reduce add-rationals x))

(defmethod + :infinite [& x]
  :infinity)

(defmethod + :float [& x]
  (->> x
       (map to-float)
       (apply core/+)))

(defmulti - should-be-rational?)

(defmethod - :integer [& x]
  (apply core/- x))

(defmethod - :empty [& x] (assert false))

(defmethod - :rational [& x]
  (let [x1 (first x)
        xs (rest x)]
    (if (empty? xs)
      (update-in x1 [:num] core/-)
      (reduce - (+ x1 (- (first xs))) (rest xs)))))

(defmethod - :infinite [& x]
  :infinity)

(defmethod - :float [& x]
  (->> x
       (map to-float)
       (apply core/-)))

(defn- comparable-rationals [x y]
  (let [denom-gcd (gcd (x :denom) (y :denom))
        lcm-multiplier-x (/ (y :denom) denom-gcd)
        lcm-multiplier-y (/ (x :denom) denom-gcd)]
    [(new-rational (* lcm-multiplier-x (x :num)) (* lcm-multiplier-x (x :denom)))
     (new-rational (* lcm-multiplier-y (y :num)) (* lcm-multiplier-y (y :denom)))]))

(defn- compare-helper [op [prev prev-val] next-val]
  (let [[comp-x comp-y] (comparable-rationals prev-val next-val)]
    (if (false? prev-val)
      [next-val false]
      [next-val (op (comp-x :num) (comp-y :num))])))

(defn- rational-compare-op [op & x]
  (->> x
       (map int->rational)
       (reduce (partial compare-helper op) [(first x) true] (rest x))))

(defmulti < should-be-rational?)

(defmethod < :rational [& x]
  (rational-compare-op core/< x))

(defmethod < :integer [& x]
  (apply core/< x))

(defmethod < :float [& x]
  (->> x
       (map to-float)
       (apply core/<)))

(defmulti > should-be-rational?)

(defmethod > :rational [& x]
  (rational-compare-op core/> x))

(defmethod > :integer [& x]
  (apply core/> x))

(defmethod > :float [& x]
  (->> x
       (map to-float)
       (apply core/>)))

(defmulti <= should-be-rational?)

(defmethod <= :rational [& x]
  (rational-compare-op core/<= x))

(defmethod <= :integer [& x]
  (apply core/> x))

(defmethod <= :float [& x]
  (->> x
       (map to-float)
       (apply core/<=)))

(defmulti >= should-be-rational?)

(defmethod >= :rational [& x]
  (rational-compare-op core/>= x))

(defmethod >= :integer [& x]
  (apply core/>= x))

(defmethod >= :float [& x]
  (->> x
       (map to-float)
       (apply core/>=)))

(defmulti = should-be-rational?)

(defmethod = :rational [& x]
  (rational-compare-op core/= x))

(defmethod = :integer [& x]
  (apply core/= x))

(defmethod = :float [& x]
  (->> x
       (map to-float)
       (apply core/=)))

(defmethod = :infinite [& x]
  (apply core/= x))

