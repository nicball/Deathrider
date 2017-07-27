(ns deathrider.server
  (:use [deathrider message gameboard player point config]
        [clojure.core.async :only [thread chan <!! >!! alts!! timeout]])
  (:require [taoensso.nippy :as nippy])
  (:import [java.net ServerSocket Socket]))

(defn- rotate-point [p]
  (new-point (point-y p) (- (point-x p))))

(defn- gen-player [id]
  (let [x (- (rand-int (/ GAMEBOARD_SIZE 2)) (/ GAMEBOARD_SIZE 4))
        p (new-point x (- (/ GAMEBOARD_SIZE 2)))
        [pos dir] (rand-nth (map vector (iterate rotate-point p)
                                        [:up :right :down :left]))]
    (new-player id pos dir)))

(defn- new-session [sock id]
  {:socket sock
   :player-id id
   :in-stream (get-data-input-stream sock)
   :out-stream (get-data-output-stream sock)})

(defn- usercmd-reader-thread [s]
  (let [ch (chan)]
    (thread
      (loop []
        (>!! ch (read-usercmd! (:in-stream s) (:player-id s)))
        (recur)))
    ch))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 SNAPSHOT_PER_SEC))
(defn serve [socks]
  (let [len (count socks)
        sessions (doall (map new-session socks (range len)))
        gb (collide (new-gameboard (map gen-player (range len))
                                   GAMEBOARD_SIZE
                                   GAMEBOARD_SIZE))]
    (println gb)

    (doseq [s sessions]
      (nippy/freeze-to-out! (:player-id s) (:out-stream s)))
    (println "Player ids sent.")

    (let [msg-chans (doall (map usercmd-reader-thread sessions))]
      (loop [moves {}
             gb gb
             to (timeout UPDATE_INTERVAL_MS)]
        (let [[msg _] (alts!! (conj msg-chans to) :priority true)]
          (cond
            (nil? msg)
            (let [new-gb (step gb moves)]
              (doseq [p (gameboard-players gb)]
                (try (nippy/freeze-to-out! (nth outs (player-id p)) (new-snapshot gb))
                  (catch java.io.IOException e
                    (println "From serve::freeze-to-out!: " e))))
              (recur {} new-gb (timeout UPDATE_INTERVAL_MS)))

            (= :quit (usercmd-type msg))
            (recur moves (mark-dead gb (usercmd-player-id msg)) to)

            (= :turn (usercmd-type msg))
            (recur (assoc moves (usercmd-player-id msg) (turn-dir msg))
                   gb
                   to)

            (has-winner? gb)
            nil

            true
            (do
              (println "Unknown usercmd" msg)
              (recur moves gb to))))))))

(defn close-socket! [^Socket s]
  (.close s))

(defn start-server []
  (let [lsn (ServerSocket. SERVER_PORT)]
    (println "Listening...")
    (loop []
      (let [socks (doall (repeatedly ROOM_SIZE #(.accept lsn)))]
        (future
          (println "Opening new room.")
          (try (serve socks)
            (catch Throwable e 
              (println "From start-server: " e))
            (finally
              (doseq [s socks] (close-socket! s)))))
        (recur)))))
