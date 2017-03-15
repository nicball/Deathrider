(ns deathrider.point)

(defrecord Point
  [x y])

(defn new-point [x y]
  (->Point x y))

(defn point-x [p]
  (:x p))

(defn point-y [p]
  (:y p))

(defn set-x [p x]
  (assoc p :x x))

(defn set-y [p y]
  (assoc p :y y))

(defn update-x [p f]
  (update p :x f))

(defn update-y [p f]
  (update p :y f))

(defn adjacent [p dir]
  (condp = dir
    :up (update-y p inc)
    :down (update-y p dec)
    :left (update-x p dec)
    :right (update-x p inc)))

(defn between? [p a b]
  (let [px (point-x p) py (point-y p)
        ax (point-x a) ay (point-y a)
        bx (point-x b) by (point-y b)]
    (or (and (<= (min ax bx) px (max ax bx))
             (== ay py by))
        (and (<= (min ay by) py (max ay by))
             (== ax px bx)))))
