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

(defn- new-usercmd [id ty data]
  {:player-id id :type ty :data data})

(defn usercmd-player-id [e] (:player-id e))

(defn usercmd-type [e] (:type e))

(defn new-turn-usercmd
  [id dir]
  (new-usercmd id :turn dir))

(defn turn-dir [e]
  (assert (= :turn (:type e)))
  (:data e))

(defn new-quit-usercmd [id]
  (new-usercmd id :quit nil))

(defn new-snapshot [players]
  {:players players})

(defn snapshot-players [s]
  (:players s))

(defn get-data-output-stream [^Socket s]
  (DataOutputStream. (.getOutputStream s)))

(defn get-data-input-stream [^Socket s]
  (DataInputStream. (.getInputStream s)))

(defn read-fully [^DataInputStream is ^bytes arr]
  (.readFully is arr))

(defn read-int [^DataInputStream is]
  (.readInt is))

(defn read-object [is]
  (let [len (read-int is)
        arr (byte-array len)]
    (println "read-object(" len ")")
    (read-fully is arr)
    (nippy/thaw arr)))

(defn read-usercmd [is id]
  (try
    (read-object is)
    (catch IOException _ (new-quit-usercmd id))))

(defn write-int [^DataOutputStream os i]
  (.writeInt os i))

(defn write-object [^DataOutputStream os object]
  (let [^bytes arr (nippy/freeze object)]
    (println "write-object(" (alength arr) ")")
    (.writeInt os (alength arr))
    (.write os arr)))

(defn flush-os [^OutputStream os]
  (.flush os))

(defn send-snapshot [outs gb]
  (doseq [p (gameboard-players gb)]
    (doto (nth outs (player-id p))
      (write-object (new-snapshot (gameboard-players gb)))
      flush-os)))

(defn close-socket [^Socket s]
  (.close s))

