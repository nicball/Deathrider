(ns deathrider.event)

(defrecord Event
  [kind player-id timestamp data])

(defn event-kind [e] (:kind e))
(defn event-player-id [e] (:player-id e))
(defn event-timestamp [e] (:timestamp e))

(defn new-born-event
  [id tm pos dir]
  (->Event :born id tm [pos dir]))
(defn born-pos [e] (first (:data e)))
(defn born-dir [e] (second (:data e)))

(defn new-turn-event
  [id tm dir]
  (->Event :turn id tm dir))
(defn turn-dir [e] (:data e))
