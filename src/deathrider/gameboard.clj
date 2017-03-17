(ns deathrider.gameboard
  (use deathrider.player))

(defrecord GameBoard
  [players])

(defn new-gameboard [players]
  (->GameBoard players))

(defn gameboard-players [gb]
  (:players gb))

(defn collide [gb]
  (new-gameboard
    (for [p (:players gb)]
      (if (and (alive? p)
               (some #(hitting? p %) (:players gb)))
        (die p)
        p))))

(defn advance [gb moves]
  (new-gameboard
    (for [p (:players gb)]
      (if (alive? p)
        (ride p (get moves (player-id p)))
        p))))

(defn step [gb moves]
  (-> gb (advance moves) collide))
