(ns deathrider.server
  (:use [deathrider message gameboard player point config]
        [clojure.core.async :only [thread <!! >!! alts!! timeout]])
  (:require [taoensso.nippy :as nippy])
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
      (nippy/freeze-to-out! (nth outs i) i)
      (flush-os! (nth outs i)))

    (loop [moves {}
           gb gb
           to (timeout UPDATE_INTERVAL_MS)]
      (let [chs (vec (conj (map #(thread (read-usercmd! %1 %2))
                                ins
                                (range len))
                           to))
            [msg _] (alts!! chs :priority true)]
        (cond
          (nil? msg)
          (let [new-gb (step gb moves)]
            (doseq [p (gameboard-players gb) :when (alive? p)]
              (doto (nth outs (player-id p))
                (nippy/freeze-to-out! (new-snapshot (gameboard-players gb)))
                flush-os!))
            (recur {} new-gb (timeout UPDATE_INTERVAL_MS)))

          (= :quit (usercmd-type msg))
          (recur moves (mark-dead gb (usercmd-player-id msg)) to)

          (= :turn (usercmd-type msg))
          (recur (assoc moves (usercmd-player-id msg) (turn-dir msg))
                 gb
                 to)

          true
          (do
            (println "Unknown usercmd" msg)
            (recur moves gb to)))))))

(defn start-server []
  (let [lsn (ServerSocket. SERVER_PORT)]
    (loop []
      (let [socks (doall (repeatedly ROOM_SIZE #(.accept lsn)))]
        (future
          (try (serve socks)
            (catch Throwable e (.printStackTrace e))
            (finally (dorun (map close-socket! socks)))))
        (recur)))))
