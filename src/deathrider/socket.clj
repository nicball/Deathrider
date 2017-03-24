(ns deathrider.socket
  (:require [clojure.core.async :as async])
  (:import [java.nio.channels
              AsynchronousSocketChannel
              AsynchronousServerSocketChannel
              InetSocketAddress
              CompletionHandler]
           [java.nio ByteBuffer]))

(def ^{:private true :const true} BUFFER_SIZE 8192)

(defn listen-sync [port]
  (let [chan (AsynchronousServerSocketChannel/open)]
    (.bind chan (InetSocketAddress. port))
    chan))

(defn- completion-handler [succ fail]
  (reify CompletionHandler
    (completed [this res data] (succ this res data))
    (failed [this e data] (fail this e data))))

(defn connect [addr port]
  (let [ch (async/chan)
        sock (AsynchronousSocketChannel/open)]
    (.connect sock (InetSocketAddress. addr port) nil
              (completion-handler
                #(async/onto-chan ch [sock])
                #(println (.getMessage %2))))
    ch))

(defn accept-chan [s]
  (let [ch (async/chan)]
    (.accept s nil
             (completion-handler
               (fn [this conn _]
                 (.accept s nil this)
                 (async/put! ch conn))
               #(println (.getMessage %2))))
    ch))

(defn receive-chan [s]
  (let [ch (async/chan)
        arr (byte-array BUFFER_SIZE)
        buf (ByteBuffer/wrap arr)]
    (.read s buf nil
           (completion-handler
             (fn [this n _]
               (if (== -1 n)
                 (async/close! ch)
                 (do
                   (async/onto-chan ch (take n arr) false)
                   (.clear buf)
                   (.read s buf nil this))))
             #(println (.getMessage %2))))
    ch))

(defn- write-and-clear-buffer [s buf]
  (let [done (async/chan)]
    (.write s buf nil
            (completion-handler
              (fn [this _ _]
                (if (.hasRemaining buf)
                  (.write s buf nil this)
                  (do
                    (async/close! done)
                    (.clear buf))))
              #(println (.getMessage %2))))
    done))

(defn send-chan [s]
  (let [ch (async/chan)
        arr (byte-array BUFFER_SIZE)
        buf (ByteBuffer/wrap arr)]
    (go-loop []
      (when-let [msg (async/<! ch)]
        (if (or (= :end-of-message msg)
                (not (.hasRemaining buf)))
          (async/<! (write-and-clear-buffer s buf))
          (.put buf msg))
        (recur)))
    ch))

(defn flush! [s]
  (async/>! s :end-of-message))

(defn close [s]
  (.close s))

