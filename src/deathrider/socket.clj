(ns deathrider.socket
  (:use [clojure.core.async :only [go go-loop chan put! <! >! >!! close!]])
  (:import [java.net InetSocketAddress]
           [java.nio.channels
              AsynchronousSocketChannel
              AsynchronousServerSocketChannel
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

(defn- print-error-and-close [ch]
  (fn [_ ^Throwable e _]
    (println (.getMessage e))
    (close! ch)))

(defn connect [^String addr ^long port]
  (let [ch (chan)
        sock (AsynchronousSocketChannel/open)]
    (.connect sock (InetSocketAddress. addr port) nil
              (completion-handler
                (fn [_ _ _]
                  (put! ch sock)
                  (close! ch))
                (print-error-and-close ch)))
    ch))

(defn accept-chan [^AsynchronousServerSocketChannel s]
  (let [ch (chan)]
    (.accept s nil
             (completion-handler
               (fn [this conn _]
                 (.accept s nil this)
                 (put! ch conn))
               (print-error-and-close ch)))
    ch))

(defn receive-chan [^AsynchronousSocketChannel s]
  (let [ch (chan)
        arr (byte-array BUFFER_SIZE)
        buf (ByteBuffer/wrap arr)]
    (.read s buf nil
           (completion-handler
             (fn [this n _]
               (if (== -1 n)
                 (close! ch)
                 (do
                   (doseq [b (take n arr)]
                     (>!! ch b))
                   (.clear buf)
                   (.read s buf nil this))))
             (print-error-and-close ch)))
    ch))

(defn- write-buffer [^AsynchronousSocketChannel s ^ByteBuffer buf]
  (let [done (chan)]
    (.write s buf nil
            (completion-handler
              (fn [this _ _]
                (if (.hasRemaining buf)
                  (.write s buf nil this)
                  (close! done)))
              (fn [this e _]
                (>!! done e)
                (close! done))))
    done))

(defn send-chan [s]
  (let [ch (chan)
        arr (byte-array BUFFER_SIZE)
        buf (ByteBuffer/wrap arr)]
    (go-loop []
      (when-let [msg (<! ch)]
        (if (or (= :end-of-message msg)
                (not (.hasRemaining buf)))
          (if-let [^Throwable e (<! (write-buffer s (ByteBuffer/wrap arr 0 (.position buf))))]
            (do (println (.getMessage e)) (close! ch))
            (recur))
          (do (.put buf (byte msg)) (recur)))))
    ch))

(defn flush-conn [s]
  (put! s :end-of-message))

(defn close [^AsynchronousSocketChannel s]
  (.close s))

