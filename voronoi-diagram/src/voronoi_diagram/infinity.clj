(ns voronoi-diagram.infinity)

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
