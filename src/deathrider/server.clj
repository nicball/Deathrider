(ns deathrider.server
  (:use [deathrider message gameboard player point]
        [clojure.core.async :only [thread <!! >!! alts!! timeout]])
  (:require [taoensso.nippy :as nippy])
  (:import [java.io InputStream
                    DataInputStream
                    OutputStream
                    DataOutputStream
                    IOException]
           [java.net ServerSocket
                     Socket]))

(defn- get-output-stream [^Socket s]
  (.getOutputStream s))
(defn- get-input-stream [^Socket s]
  (.getInputStream s))
(defn- read-all [^InputStream is ^bytes arr]
  (loop [pos 0]
    (when-not (== pos (alength arr))
      (let [n (.read is arr pos (- (alength arr) pos))]
        (if (== -1 n)
          (throw (java.io.IOException. "deathrider.server/read-all: not enough data."))
          (recur (+ pos n)))))))
(defn- read-int [^DataInputStream is]
  (.readInt is))
(defn- read-object [is]
  (let [len (read-int is)
        arr (byte-array len)]
    (read-all is arr)
    (nippy/thaw arr)))
(defn- write-int [^DataOutputStream os i]
  (.writeInt os i))
(defn- write-object [^DataOutputStream os object]
  (let [^bytes arr (nippy/freeze object)]
    (.writeInt os (alength arr))
    (.write os arr)))
(defn- flush-os [^OutputStream os]
  (.flush os))
(defn- send-snapshot [outs gb]
  (doseq [p (gameboard-players gb)]
    (doto (nth outs (player-id p))
      (write-object (gameboard-players gb))
      flush-os)))

(def ^:private GAMEBOARD_SIZE 10)
(defn- rand-pos []
  (new-point (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))
             (- (rand-int GAMEBOARD_SIZE) (/ GAMEBOARD_SIZE 2))))
(defn- gen-player [id]
  (new-player id (rand-pos) (rand-nth [:up :down :left :right])))

(def ^:private UPDATE_INTERVAL_MS (/ 1000 2))
(defn serve [socks]
  (let [len (count socks)
        outs (doall (map #(DataOutputStream. (get-output-stream %)) socks))
        ins (doall (map #(DataInputStream. (get-input-stream %)) socks))
        gb (new-gameboard (map gen-player (range len)))]
    (println "Len: " len)
    (dotimes [i len]
      (write-int (nth outs i) i)
      (flush-os (nth outs i))
      (println "Player-ids sent."))
    (loop [moves {}
           gb gb
           to (timeout UPDATE_INTERVAL_MS)]
      (let [chs (vec (conj (map #(thread (read-object %)) ins) to))
            [val _] (alts!! chs :priority true)]
        (println "Received: " val)
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
      (let [socks (doall (repeatedly ROOM_SIZE (fn [] (.accept lsn))))]
        (println "Socks: " socks)
        (try (serve socks)
          (finally (doall (map close-socket socks))))))))

