(ns deathrider.server
  (:use [deathrider message gameboard player point]
        [clojure.core.async :only [thread <!! >!! alts!! timeout]])
  (:import [java.net ServerSocket]))

(def ^:private GAMEBOARD_SIZE 10)

(defn- rand-pos []
  (new-point (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))
             (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))))

(defn- gen-player [id]
  (new-player id (rand-pos) (rand-nth [:up :down :left :right])))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 2))

(defn serve [socks]
  (let [len (count socks)
        outs (doall (map get-data-output-stream socks))
        ins (doall (map get-data-input-stream socks))
        gb (new-gameboard (map gen-player (range len)))]

    (dotimes [i len]
      (write-int (nth outs i) i)
      (flush-os (nth outs i)))

    (loop [moves {}
           gb gb
           to (timeout UPDATE_INTERVAL_MS)]
      (let [chs (vec (conj (map #(thread (read-usercmd %)) ins) to))
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

(def ^:private ROOM_SIZE 2)
(def PORT 46666)

(defn start-server []
  (let [lsn (ServerSocket. PORT)]
    (loop []
      (let [socks (doall (repeatedly ROOM_SIZE (fn [] (.accept lsn))))]
        (try (serve socks)
          (finally (doall (map close-socket socks))))))))

