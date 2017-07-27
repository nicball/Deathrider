(ns deathrider.server
  (:use [deathrider message gameboard player point config]
        [clojure.core.async :only [thread chan close! <!! >!! alts!! timeout]])
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
   :out-stream (get-data-output-stream sock)
   :quited false})

(defn- usercmd-reader-thread [s]
  (let [ch (chan)]
    (thread
      (loop []
        (let [cmd (read-usercmd! (:in-stream s) (:player-id s))]
          (>!! ch cmd)
          (when-not (= :quit (usercmd-type cmd))
            (recur)))))
    ch))

(defn- send-snapshot [ss id gb]
  (let [s (first (drop-while #(not= id (:player-id %)) ss))]
    (when-not (:quited s)
      (nippy/freeze-to-out! (:out-stream s) (new-snapshot gb)))))

(defn- mark-quited! [ss id]
  (map (fn [s]
         (if (= id (:player-id s))
           (assoc s :quited true)
           s)
       ss))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 SNAPSHOT_PER_SEC))
(defn serve [socks]
  (let [len (count socks)
        sessions (doall (map new-session socks (range len)))
        gb (collide (new-gameboard (map gen-player (range len))
                                   GAMEBOARD_SIZE
                                   GAMEBOARD_SIZE))]
    (println gb)

    (doseq [s sessions]
      (nippy/freeze-to-out! (:out-stream s) (:player-id s)))
    (println "Player ids sent.")

    (let [msg-chans (doall (map usercmd-reader-thread sessions))]
      (try
        (loop [moves {}
               gb gb
               to (timeout UPDATE_INTERVAL_MS)
               sessions sessions]
          (let [[msg _] (alts!! (conj msg-chans to) :priority true)]
            (cond
              (nil? msg)
              (let [new-gb (step gb moves)]
                (doseq [p (gameboard-players gb)]
                  (try (send-snapshot sessions (player-id p) gb)
                    (catch java.io.IOException e
                      (println "From serve::freeze-to-out!: " e))))
                (recur {} new-gb (timeout UPDATE_INTERVAL_MS) sessions))

              (= :quit (usercmd-type msg))
              (recur moves (mark-dead gb (usercmd-player-id msg)) to
                     (mark-quited sessions (usercmd-player-id msg))

              (= :turn (usercmd-type msg))
              (recur (assoc moves (usercmd-player-id msg) (turn-dir msg))
                     gb to sessions)

              (or (has-winner? gb) (every? (complement alive?) (gameboard-players gb)))
              nil

              true
              (do
                (println "Unknown usercmd" msg)
                (recur moves gb to sessions)))))
        (finally
          (doseq [c msg-chans]
            (close! c)))))))
      

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
