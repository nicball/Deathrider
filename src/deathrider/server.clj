(ns deathrider.server
  (:use [deathrider message gameboard]
        [clojure.core.async :only [thread <!! >!! alts!!]])
  (:require [taoensso.nippy :as nippy])
  (:import [java.io InputStream
                    DataInputStream
                    OutputStream
                    DataOutputStream
                    IOException]
           [java.net ServerSocket
                     Socket]))

(defn- read-all [^InputStream is arr]
  (loop [pos 0]
    (when-not (== pos (alength arr))
      (let [n (.read is arr pos (- (alength arr) pos))]
        (if (== -1 n)
          (throw (java.io.IOException. "deathrider.server/read-all: not enough data."))
          (recur (+ pos n)))))))
(defn- read-int [^DataInputStream is]
  (.readInt is))
(defn- read-object [is]
  (thread
    (let [len (read-int is)
          arr (byte-arr len)]
      (read-all is arr)
      (nippy/thaw arr))))
(defn- write-int [^DataOutputStream os i]
  (.writeInt os i))
(defn- write-object [^DataOutputStream os object]
  (let [arr (nippy/freeze object)]
    (.writeInt os (alength arr))
    (.write os arr)))
(defn- send-snapshot [outs gb]
  (doseq [p (gameboard-players gb)]
    (write-object (nth outs (player-id p)) (gameboard-players gb))))

(def ^:private GAMEBOARD_SIZE 10)
(defn- rand-pos []
  (new-point (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))
             (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))))
(defn- gen-player [id]
  (new-player id (rand-pos) (rand-nth [:up :down :left :right])))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 2))
(defn serve [socks]
  (let [len (count socks)
        outs (dorun (map #(DataOutputStream. (.getOutputStream %)) socks))
        ins (dorun (map #(DataInputStream. (.getInputStream %)) socks))
        gb (new-gameboard (map gen-player (range len)))]
    (dotimes [i len]
      (write-int (nth outs i) i))
    (loop [moves {}
           gb gb
           to (timeout UPDATE_INTERVAL_MS)]
      (let [[val _] (alts!! (conj to
                                  (map #(thread (read-object %)) ins))
                            :priority true)]
        (if (nil? val)
          (do (send-snapshot outs gb) 
              (recur {} (step gb moves) (timeout UPDATE_INTERVAL_MS)))
          (recur (assoc moves (usercmd-player-id val) (turn-dir val))
                 gb
                 to))))))

(defn- close-socket [^Socket s]
  (.close s))
(def ^:private ROOM_SIZE 2)
(defn start-server [port]
  (let [lsn (ServerSocket. port)]
    (loop []
      (let [socks (dorun (repeatedly ROOM_SIZE (fn [] (.accept lsn))))]
        (try (serve socks)
          (finally (dorun (map close-socket socks))))))))
