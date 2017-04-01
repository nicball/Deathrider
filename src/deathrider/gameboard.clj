(ns deathrider.gameboard
  (:use [deathrider player point]))

(defrecord GameBoard
  [players width height])

(defn new-gameboard [players width height]
  (->GameBoard players width height))

(defn with-players [gb players]
  (->GameBoard players (:width gb) (:height gb)))

(defn gameboard-players [gb]
  (:players gb))

(defn- out-of-border? [p gb]
  (let [h (player-head p)
        ^long x (point-x h) ^long y (point-y h)
        x-max (/ (:width gb) 2)
        y-max (/ (:height gb) 2)]
    (or (> (Math/abs x) x-max)
        (> (Math/abs y) y-max))))

(defn collide [gb]
  (with-players gb
    (for [p (:players gb)]
      (if (and (alive? p)
               (or (some #(hitting? p %) (:players gb))
                   (out-of-border? p gb)))
        (die p)
        p))))

(defn advance [gb moves]
  (with-players gb
    (for [p (:players gb)]
      (if (alive? p)
        (ride p (get moves (player-id p)))
        p))))

(defn mark-dead [gb id]
  (with-players gb
    (map (fn [p] (if (alive? p) (die p) p))
         (:players gb))))

(defn step [gb moves]
  (-> gb collide (advance moves)))
