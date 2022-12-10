(defn parse [line] [(apply str (take 1 line)) (Integer. (apply str (drop 2 line)))])
(defn expand [[dir dist]] (repeat dist dir))
(defn absdiff [a b] (abs (- a b)))
(defn diagonal [[tx ty] [hx hy]] (and (not= tx hx) (not= ty hy)))
(defn distance [[tx ty] [hx hy]] (max (absdiff tx hx) (absdiff ty hy)))
(defn signum [x] (if (= 0 x) 0 (/ x (abs x))))
(defn move-towards [[tx ty] [hx hy]] [(+ tx (signum (- hx tx))) (+ ty (signum (- hy ty)))])

(defn chase [tail head]
    (if (< 1 (distance tail head))
        [(move-towards tail head) head]
        [tail head]))

(defmulti follow (fn [_ d] d))
(defmethod follow "R" [[tail [x y]] _] (chase tail [(+ x 1) y]))
(defmethod follow "L" [[tail [x y]] _] (chase tail [(- x 1) y]))
(defmethod follow "U" [[tail [x y]] _] (chase tail [x (- y 1)]))
(defmethod follow "D" [[tail [x y]] _] (chase tail [x (+ y 1)]))

(println (count (set (map first (reductions follow [[0 0] [0 0]] (mapcat expand (map parse (line-seq (java.io.BufferedReader. *in*)))))))))
