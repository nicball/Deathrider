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

(defn new-join-usercmd []
  (->UserCommand 0 :join nil))

(defrecord Snapshot
  [players])

(defn new-snapshot [players]
  (->Snapshot players))

(defn snapshot-players [s]
  (:players s))


(defn write-usercmd! [ch cmd]
    (>! ch (int (:player-id cmd)))
    (>! ch (condp = (:type cmd)
                :join 0 :turn 1))
    (when (= :turn (:type cmd))
      (>! ch (condp = (:data cmd)
                  :up 0 :down 1 :left 2 :right 3))))

(defn read-usercmd! [ch]
  (let [id (<! ch)
        type (condp = (<! ch)
               0 :join 1 :turn)]
    (when (== -1 id)
      (throw (IllegalArgumentException. "Cannot read player id: connection closed.")))
    (if (= :join type)
      (new-join-usercmd)
      (let [dir (condp = (<! ch)
                  0 :up 1 :down 2 :left 3 :right)]
        (new-turn-usercmd id dir)))))

(defn- write-big-endian-int! [ch ^int i]
  (doto ch
    (>! (bit-shift-right (bit-and i 0xFF000000) 24))
    (>! (bit-shift-right (bit-and i 0x00FF0000) 16))
    (>! (bit-shift-right (bit-and i 0x0000FF00) 8))
    (>! (bit-shift-right (bit-and i 0x000000FF) 0))))

(defn- write-point! [ch point]
  (doto ch
    (write-big-endian-int! (point-x point))
    (write-big-endian-int! (point-y point))))

(defn- write-track! [ch track]
  (write-big-endian-int! ch (count track))
  (doseq [p track]
    (write-point! ch p)))

(defn- write-player! [ch player]
  (doto ch
    (>! (player-id player))
    (>! (condp = (player-status player)
              :alive 0 :dead 1))
    (write-track! (player-track player))))

(defn write-snapshot! [ch snapshot]
  (write-big-endian-int (count (:players snapshot)))
  (doseq [p (:players snapshot)]
    (write-player! ch p)))

