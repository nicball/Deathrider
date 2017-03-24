(ns deathrider.socket
  (:use [clojure.core.async :only [chan put! <! >! close!]])
  (:import [java.net InetSocketAddress]
           [java.nio.channels
              AsynchronousSocketChannel
              AsynchronousServerSocketChannel
              CompletionHandler]
           [java.nio ByteBuffer]))

(def ^{:private true :const true} BUFFER_#SIZE 8192)

(defn listen [port]
  (let [chan (AsynchronousServerSocketChannel/open)]
    (.bind chan (InetSocketAddress. port))
    chan))

(defn completion-handler [succ fail]
  (reify CompletionHandler
    (completed [this res data] (succ this res data))
    (failed [this e data] (fail this e data))))

(defmacro connect! [addr port]
  `(let [ch# (chan)
         sock# (AsynchronousSocketChannel/open)]
     (.connect sock# (InetSocketAddress. ~addr ~port) nil
               (completion-handler
                 (fn [_1# _2# _3#] (close! ch#))
                 (fn [_1# e# _3#]
                   (put! ch# e#)
                   (close! ch#))))
     (if-let [error# (<! ch#)]
       (throw error#)
       sock#)))

(defmacro accept! [s]
  `(let [ch# (chan)]
     (.accept ~s nil
              (completion-handler
                (fn [_1# conn# _3#] (put! ch# conn#))
                (fn [_1# e# _3#] (put! ch# e#))))
     (let [res# (<! ch#)]
       (if (instance? Throwable res#)
         (throw res#)
         res#))))

(defmacro receive! [s arr]
  `(let [ch# (chan)
         buf# (ByteBuffer/wrap ~arr)]
     (.read ~s buf# nil
            (completion-handler
              (fn [_1# n# _3#] (put! ch# n#))
              (fn [_1# e# _3#] (put! ch# e#))))
     (let [res# (<! ch#)]
       (if (instance? Throwable res#)
         (throw res#)
         res#))))

(defmacro receive-byte! [s]
  `(let [s# ~s
         ch# (chan)
         arr# (byte-array 1)
         buf# (ByteBuffer/wrap arr)]
     (.read s# buf# nil
            (completion-handler
              (fn [this# n# _3#]
                (cond
                  (== -1 n#) (put! ch# :end-of-stream)
                  (.hasRemaining buf#) (.read s# buf# this#)
                  true (put! ch# (aget arr 0))))
              (fn [_1# e# _3#] (put! ch# e#))))
     (let [res# (<! ch#)]
       (if (instance? Throwable res#)
         (throw res#)
         res#))))

(defmacro send! [s arr offset length]
  `(let [ch# (chan)
         buf# (ByteBuffer/wrap ~arr ~offset ~length)]
     (.write ~s buf# nil
             (completion-handler
               (fn [_1# n# _3#] (put! ch# n#))
               (fn [_1# e# _3#] (put! ch# e#))))
     (let [res# (<! ch#)]
       (if (instance? Throwable res#)
         (throw res#)
         res#))))

(defmacro send-all! [s arr]
  `(let [s# ~s
         ch# (chan)
         buf# (ByteBuffer/wrap ~arr)]
     (.write s# buf# nil
             (completion-handler
               (fn [this# _2# _3#]
                 (if (.hasRemaining buf#)
                   (.write s# buf# nil this#)
                   (close! ch#)))
               (fn [_1# e# _3#]
                 (put! ch# e#))))
     (when-let [error# (<! ch#)]
       (throw error#))))

(defmacro send-byte! [s b]
  `(send-all! ~s (byte-array [~b])))

(defn close [^AsynchronousSocketChannel s]
  (.close s))

