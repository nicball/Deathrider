(ns deathrider.socket
  (:require [clojure.core.async :as async])
  (:import [java.nio.channels
              AsynchronousSocketChannel
              AsynchronousServerSocketChannel
              InetSocketAddress
              CompletionHandler]
           [java.nio ByteBuffer]))

(def ^{:private true :const true} BUFFER_SIZE 8192)

(defn listen [port]
  (let [chan (AsynchronousServerSocketChannel/open)]
    (.bind chan (InetSocketAddress. port))
    chan))

(defn- completion-handler [succ fail]
  (reify CompletionHandler
    (completed [this res data] (succ this res data))
    (failed [this e data] (fail this e data))))

(defn go-connect [addr port]
  (let [ch (async/chan)
        sock (AsynchronousSocketChannel/open)]
    (.connect sock (InetSocketAddress. addr port) nil
              (completion-handler
                #(async/put! ch sock)
                #(println (.getMessage %2))))
    ch))

(defn go-accept [s]
  (let [ch (async/chan)]
    (.accept s nil
             (completion-handler
               (fn [this conn _]
                 (.accept s nil this)
                 (async/put! ch conn))
               #(println (.getMessage %2))))
    ch))

(defn go-receive [s]
  (let [ch (async/chan)
        arr (byte-array BUFFER_SIZE)]
    (.read s (ByteBuffer/wrap arr) nil
           (completion-handler
             (fn [this n _]
               (if (== -1 n)
                 (async/close! ch)
                 (do
                   (async/onto-chan ch (take n arr))
                   (.read s (ByteBuffer/wrap arr) nil this))))
             #(println (.getMessage %2))))
    ch))

(defn go-send [s data]
  (let [ch (async/chan)
        buf (ByteBuffer/wrap data)]
    (.write s buf nil
            (completion-handler
              (fn [this _ _]
                (if (.hasRemaining buf)
                  (.write s buf nil this)
                  (async/close! ch)))
              #(println (.getMessage %2))))
    ch))

