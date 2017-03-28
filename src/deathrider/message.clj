(ns deathrider.message
  (:use [clojure.core.async :only [>! <!]]))

(defrecord UserCommand
  [player-id type data])
(defn usercmd-player-id [e] (:player-id e))
(defn usercmd-type [e] (:type e))
(defn new-turn-usercmd
  [id dir]
  (->UserCommand id :turn dir))
(defn turn-dir [e]
  (assert (= :turn (:type e)))
  (:data e))

(defrecord Reply
  [type data])
(defn new-welcome-reply [id pos]
  (->Reply :welcome [id pos]))
(defn welcome-id [r]
  (assert (= :welcome (:type r)))
  (first (:data r)))
(defn welcome-pos [r]
  (assert (= :welcome (:type r)))
  (second (:data r)))

(defrecord Snapshot
  [players])
(defn new-snapshot [players]
  (->Snapshot players))
(defn snapshot-players [s]
  (:players s))
