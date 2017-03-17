(ns deathrider.message
  (:import [java.io OutputStream]))

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


(defn write-usercmd! [^OutputStream s cmd]
    (.write s (int (:player-id cmd)))
    (.write s (condp = (:type cmd)
                :join 0 :turn 1))
    (when (= :turn (:type cmd))
      (.write s (condp = (:data cmd)
                  :up 0 :down 1 :left 2 :right 3))))

(defn read-usercmd! [^InputStream s]
  (let [id (.read s)
        type (condp = (.read s)
               0 :join 1 :turn)]
    (when (== -1 id)
      (throw (IllegalArgumentException. "Cannot read player id: connection closed.")))
    (if (= :join type)
      (new-join-usercmd)
      (let [dir (condp = (.read s)
                  0 :up 1 :down 2 :left 3 :right)]
        (new-turn-usercmd id dir)))))

(defn- write-big-endian-int! [^OutputStream s ^int i]
  (doto s
    (.write (bit-shift-right (bit-and i 0xFF000000) 24))
    (.write (bit-shift-right (bit-and i 0x00FF0000) 16))
    (.write (bit-shift-right (bit-and i 0x0000FF00) 8))
    (.write (bit-shift-right (bit-and i 0x000000FF) 0))))

(defn- write-point! [^OutputStream s point]
  (doto s
    (write-big-endian-int! (point-x point))
    (write-big-endian-int! (point-y point))))

(defn- write-track! [^OutputStream s track]
  (write-big-endian-int! s (count track))
  (doseq [p track]
    (write-point! s p)))

(defn- write-player! [^OutputStream s player]
  (doto s
    (.write (player-id player))
    (.write (condp = (player-status player)
              :alive 0 :dead 1))
    (write-track! (player-track player))))

(defn write-snapshot! [^OutputStream s snapshot]
  (write-big-endian-int (count (:players snapshot)))
  (doseq [p (:players snapshot)]
    (write-player! s p)))

