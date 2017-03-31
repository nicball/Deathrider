(ns deathrider.message
  (:use [clojure.core.async :only [>! <!]]
        [deathrider gameboard player])
  (:require [taoensso.nippy :as nippy])
  (:import [java.io InputStream
                    DataInputStream
                    OutputStream
                    DataOutputStream
                    IOException]
           [java.net Socket]))

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

(defn new-quit-usercmd [id]
  (->UserCommand id :quit nil))

(defrecord Reply
  [type data])

(defn new-welcome-reply [id pos]
  (->Reply :welcome [id pos]))

(defn welcome-id [r]
  (assert (= :welcome (:type r)))
  (first (:data r)))

(defn welcome-pos [r]
  (assert (= :welcome (:type r)))
  (second (:data r)))

(defrecord Snapshot
  [players])

(defn new-snapshot [players]
  (->Snapshot players))

(defn snapshot-players [s]
  (:players s))

(defn get-data-output-stream [^Socket s]
  (DataOutputStream. (.getOutputStream s)))

(defn get-data-input-stream [^Socket s]
  (DataInputStream. (.getInputStream s)))

(defn read-all [^InputStream is ^bytes arr]
  (loop [pos 0]
    (when-not (== pos (alength arr))
      (let [n (.read is arr pos (- (alength arr) pos))]
        (if (== -1 n)
          (throw (java.io.IOException. "deathrider.server/read-all: not enough data."))
          (recur (+ pos n)))))))

(defn read-int [^DataInputStream is]
  (.readInt is))

(defn read-object [is]
  (let [len (read-int is)
        arr (byte-array len)]
    (read-all is arr)
    (nippy/thaw arr)))

(defn read-usercmd [is id]
  (try
    (read-object is)
    (catch IOException _ (new-quit-usercmd id))))

(defn write-int [^DataOutputStream os i]
  (.writeInt os i))

(defn write-object [^DataOutputStream os object]
  (let [^bytes arr (nippy/freeze object)]
    (.writeInt os (alength arr))
    (.write os arr)))

(defn flush-os [^OutputStream os]
  (.flush os))

(defn send-snapshot [outs gb]
  (doseq [p (gameboard-players gb)]
    (doto (nth outs (player-id p))
      (write-object (gameboard-players gb))
      flush-os)))

(defn close-socket [^Socket s]
  (.close s))

