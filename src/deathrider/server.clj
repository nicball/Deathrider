(ns deathrider.server
  (:use [deathrider message gameboard player point config]
        [clojure.core.async :only [thread <!! >!! alts!! timeout]])
  (:import [java.net ServerSocket]))

(defn- rotate-point [p]
  (new-point (point-y p) (- (point-x p))))

(defn- gen-player [id]
  (let [x (- (rand-int (/ GAMEBOARD_SIZE 2)) (/ GAMEBOARD_SIZE 4))
        p (new-point x (- (/ GAMEBOARD_SIZE 2)))
        [pos dir] (rand-nth (map vector (iterate rotate-point p)
                                        [:up :right :down :left]))]
    (new-player id pos dir)))

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
        (println "Received: " val)
        (println "Moves: " moves)
        (cond
          (nil? val)
          (let [new-gb (step gb moves)]
            (send-snapshot outs new-gb) 
            (recur {} new-gb (timeout UPDATE_INTERVAL_MS)))

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
