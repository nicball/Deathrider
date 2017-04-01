(ns deathrider.gameboard
  (use deathrider.player))

(defrecord GameBoard
  [players width height])

(defn new-gameboard [players width height]
  (->GameBoard players width height))

(defn gameboard-players [gb]
  (:players gb))

(defn- out-of-border? [p gb]
  (let [h (player-head p)
        x (point-x h) y (point-y h)
        x-max (/ (:width gb) 2)
        y-max (/ (:height gb) 2)]
    (or (> (Math/abs x) x-max)
        (> (Math/abs y) y-max))))

(defn collide [gb]
  (new-gameboard
    (for [p (:players gb)]
      (if (and (alive? p)
               (or (some #(hitting? p %) (:players gb))
                   (out-of-border? p gb)))
        (die p)
        p))))

(defn advance [gb moves]
  (new-gameboard
    (for [p (:players gb)]
      (if (alive? p)
        (ride p (get moves (player-id p)))
        p))))

(defn mark-dead [gb id]
  (new-gameboard
    (map (fn [p] (if (alive? p) (die p) p))
         (:players gb))))

(defn step [gb moves]
  (-> gb (advance moves) collide))
