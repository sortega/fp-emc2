(ns mirrorhell.core)

(defn gcd [a b]
  (cond
    (neg? a)   (recur (- a) b)
    (neg? b)   (recur a (- b))
    (< a b)    (recur b a)
    (zero? b)  a
    :else      (recur b (mod a b))))

(defn directions [d]
  (let [interval (range (- d) (inc d))]
    (set
      (for [x interval y interval
            :when (not (= x y 0))
            :let [g (gcd x y)]]
        [(/ x g) (/ y g)]))))

(defn dist2 "Non euclidean dist"
  [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2), dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn dist [a b]
  (Math/sqrt (double (dist2 a b))))

(defn dir-vector [a b] (map - b a))

(defn point-in-ray? [p {a :orig, b :dir}]
  (let [[bx by] (dir-vector a b)
        [px py] (dir-vector a p)
        t (cond 
            (== bx px 0)              (/ py by)
            (== by py 0)              (/ px bx)
            (== (/ px bx) (/ py by))  (/ px bx)
            :else                     0)]
    (pos? t)))

(defn relative-cut [[[x1 y1] [x2 y2]]
                    [[x3 y3] [x4 y4]]]
  (let [denom (- (* (- y4 y3) (- x2 x1))
                 (* (- x4 x3) (- y2 y1)))]
    (when (not (zero? denom))
      [(/ (- (* (- x4 x3) (- y1 y3))
             (* (- y4 y3) (- x1 x3)))
          denom)
       (/ (- (* (- x2 x1) (- y1 y3))
             (* (- y2 y1) (- x1 x3)))
          denom)])))

(defn relative-point [[[x1 y1] _ :as segment] t]
  (let [[ux uy] (apply dir-vector segment)]
    [(+ x1 (* t ux))
     (+ y1 (* t uy))]))

(defn intersect-segment [{:keys [orig dir]} segment]
  (when-let [[tr ts] (relative-cut [orig dir] segment)]
    (when (and (pos? tr)    ; in front of the ray
               (<= 0 ts 1)) ; inside the segment
      (relative-point [orig dir] tr))))

(defn raytrace [{:keys [orig dir] :as ray}
                {:keys [center segments] :as room}]
  (let [cuts (-> (group-by #(intersect-segment ray %) segments)
               (dissoc nil))
        closest-cut (apply min-key
                           (comp #(dist2 orig %) first)
                           cuts)]
    (if (point-in-ray? center ray)
      {:reflex center}
      (zipmap [:cut :segments] closest-cut))))

(defn vec-comp [& fns]
  (fn [avect] (vec (map #(%1 %2) fns avect))))

(defn flipper [c]
  (fn [x] (+ c (- c x))))

(defn point-flipper [[[ax ay] [bx by]]]
  (if (= ax bx)
    (vec-comp (flipper ax) identity)
    (vec-comp identity     (flipper ay))))

(defn segment-flipper [segment]
  (let [flipp (point-flipper segment)]
    (vec-comp flipp flipp)))

(defn mirror [axis {:keys [center segments]}]
  (let [pflip (point-flipper axis)
        sflip (segment-flipper axis)]
    {:center   (pflip center)
     :segments (map sflip segments)}))

(defn rot90 [[x y]] [(- y) x])

(defn concave-from? [segments p]
  (let [{[[a _]] 2 [[b _] [c _]] 1} (group-by second (frequencies (apply concat segments)))]
    (loop [ab (dir-vector a b)
           ac (dir-vector a c)
           ap (dir-vector a p)]
      (if (not-any? neg? (flatten [ab ac]))
        (not-any? neg? ap)
        (recur (rot90 ab) (rot90 ac) (rot90 ap))))))

(defn make-ray 
  ([x1 y1 x2 y2] {:orig [x1 y1]
                  :dir  [x2 y2]})
  ([orig dir]    {:orig orig
                  :dir  dir}))

(defn reflect [d {:keys [orig dir] :as ray} room]
  (println (format "reflect %s [%s %s] into room with center %s"
                   d (vec orig) (vec dir) (:center room)))
  (let [dir        (dir-vector orig dir)
        {:keys [reflex segments cut]} (raytrace ray room)]
    (if reflex reflex
      (let [rest-d       (- d (dist orig cut))
            advanced-ray (make-ray cut (map + cut dir))]
        (case (count segments)
          1  (recur rest-d advanced-ray (mirror (first segments) room))
          2  (when (concave-from? segments orig)
               (recur rest-d advanced-ray
                      (->> room
                        (mirror (first segments))
                        (mirror (second segments)))))
          )))))

(defn reflexes [d {:keys [center] :as room}]
  (set
    (filter (fn [dir] (reflect d [center dir] room)) 
            (directions d))))

;; test data

(def sq-room
  {:segments [[[0 0] [0 2]]
              [[0 2] [2 2]]
              [[2 2] [2 0]]
              [[2 0] [0 0]]]
   :center   [1 1]})

(def column-room
  {:segments [[[ 0 0] [ 0 6]]
              [[ 0 6] [10 6]]
              [[10 6] [10 0]]
              [[10 0] [ 0 0]]

              [[ 4 3] [ 4 4]]
              [[ 4 4] [ 5 4]]
              [[ 5 4] [ 5 3]]
              [[ 5 3] [ 4 3]]]
   :center   [3.5 1.5]})
