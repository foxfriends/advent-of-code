(defn parse [line] [(apply str (take 1 line)) (Integer. (apply str (drop 2 line)))])
(defn expand [[dir dist]] (repeat dist dir))
(defn absdiff [a b] (abs (- a b)))
(defn diagonal [[tx ty] [hx hy]] (and (not= tx hx) (not= ty hy)))
(defn distance [[tx ty] [hx hy]] (max (absdiff tx hx) (absdiff ty hy)))
(defn signum [x] (if (= 0 x) 0 (/ x (abs x))))
(defn move-towards [[tx ty] [hx hy]] [(+ tx (signum (- hx tx))) (+ ty (signum (- hy ty)))])

(defn chase [tail head]
    (if (< 1 (distance tail head))
        (move-towards tail head)
        tail))

(defmulti move (fn [_ d] d))
(defmethod move "R" [[x y] _] [(+ x 1) y])
(defmethod move "L" [[x y] _] [(- x 1) y])
(defmethod move "U" [[x y] _] [x (- y 1)])
(defmethod move "D" [[x y] _] [x (+ y 1)])

(defn followchain [chain d] (reductions #(chase %2 %1) (move (first chain) d) (rest chain)))
(println (count (set (map last (reductions followchain (repeat 10 [0 0]) (mapcat expand (map parse (line-seq (java.io.BufferedReader. *in*)))))))))
