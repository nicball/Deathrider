(ns deathrider.server
  (:use [deathrider message gameboard player point config]
        [clojure.core.async :only [thread <!! >!! alts!! timeout]])
  (:import [java.net ServerSocket]))

(defn- rand-pos []
  (new-point (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))
             (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))))

(defn- gen-player [id]
  (new-player id (rand-pos) (rand-nth [:up :down :left :right])))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 SNAPSHOT_PER_SEC))
(defn serve [socks]
  (let [len (count socks)
        outs (doall (map get-data-output-stream socks))
        ins (doall (map get-data-input-stream socks))
        gb (collide (new-gameboard (map gen-player (range len))
                                   GAMEBOARD_SIZE
                                   GAMEBOARD_SIZE))]
    (println gb)

    (dotimes [i len]
      (write-int (nth outs i) i)
      (flush-os (nth outs i)))

    (loop [moves {}
           gb gb
           to (timeout UPDATE_INTERVAL_MS)]
      (let [chs (vec (conj (map #(thread (read-usercmd %1 %2))
                                ins
                                (range len))
                           to))
            [val _] (alts!! chs :priority true)]
        (cond
          (nil? val)
          (do (send-snapshot outs gb) 
              (recur {} (step gb moves) (timeout UPDATE_INTERVAL_MS)))

          (= :quit (usercmd-type val))
          (recur moves (mark-dead gb (usercmd-player-id val)) to)

          true
          (recur (assoc moves (usercmd-player-id val) (turn-dir val))
                 gb
                 to))))))

(defn start-server []
  (let [lsn (ServerSocket. SERVER_PORT)]
    (loop []
      (let [socks (doall (repeatedly ROOM_SIZE #(.accept lsn)))]
        (future
          (try (serve socks)
            (catch Throwable e (.printStackTrace e))
            (finally (dorun (map close-socket socks)))))
        (recur)))))
