(ns deathrider.client
  (:use [seesaw core graphics]
        [deathrider config gameboard player point message])
  (:require [taoensso.nippy :as nippy])
  (:import [java.net Socket]
           [java.awt.event KeyEvent]))

(def ^{:private true :tag String} SERVER_HOSTNAME "localhost")
(def ^:private CANVAS_SIZE 1000)
(def ^:private UNIT_SIZE (/ CANVAS_SIZE (inc GAMEBOARD_SIZE)))

(defn- welcome-painter [_ g]
  (draw g (string-shape 5 5 "正在连接服务器")
          (style :foreground :white)))

(defn- new-canvas []
  (let [cv (canvas :background :black :paint welcome-painter
                   :size [CANVAS_SIZE :by CANVAS_SIZE])
        fr (frame :title "deathrider"
                  :content cv
                  ;;:resizable? false
                  :on-close :exit)]
    (-> fr pack! show! invoke-later)
    cv))

(def ^:private color-map
  [:lime :red :aqua :yellow :blue :fuchsia :teal :green])

(defn- paint-background [g]
  (dotimes [i GAMEBOARD_SIZE]
    (let [p (* (inc i) UNIT_SIZE)]
      (draw g
        (line p UNIT_SIZE p (- CANVAS_SIZE UNIT_SIZE))
        (style :foreground :white)
        (line UNIT_SIZE p (- CANVAS_SIZE UNIT_SIZE) p)
        (style :foreground :white)))))

(defn- to-screen-coord [p]
  (let [x (point-x p) y (point-y p)]
    (new-point (* UNIT_SIZE (+ 1 x (long (/ GAMEBOARD_SIZE 2))))
               (* UNIT_SIZE (+ 1 (- y) (long (/ GAMEBOARD_SIZE 2)))))))

(defn- paint-player [g player]
  (let [cl (get color-map (player-id player) :green)
        track (map to-screen-coord (player-track player))]
    (draw g (circle (point-x (first track))
                    (point-y (first track))
                    10)
            (style :foreground cl :background cl))
    (doseq [[start end] (map vector track (rest track))]
      (draw g
        (line (point-x start) (point-y start)
              (point-x end) (point-y end))
        (style :foreground cl :stroke 5)))))

(defn- painter [players]
  (fn [_ g]
    (paint-background g)
    (dorun (map #(paint-player g %) players))))

(defn- reflect-snapshots [is cv]
  (loop []
    (let [ss (nippy/thaw-from-in! is)]
      (println ss)
      (config! cv :paint (painter (snapshot-players ss)))
      (repaint! cv)
      (recur))))

(defn start-client []
  (let [cv (new-canvas)]
    (try
      (let [sock (Socket. SERVER_HOSTNAME SERVER_PORT)
            is (get-data-input-stream sock)
            os (get-data-output-stream sock)
            id (nippy/thaw-from-in! is)]
        (println "id:" id)
        (listen (to-root cv) :key-pressed
          (fn [^KeyEvent e]
            (when-let [dir
                       (condp = (.getKeyCode e)
                         KeyEvent/VK_UP :up
                         KeyEvent/VK_DOWN :down
                         KeyEvent/VK_LEFT :left
                         KeyEvent/VK_RIGHT :right
                         nil)]
              (println (new-turn-usercmd id dir))
              (nippy/freeze-to-out! os (new-turn-usercmd id dir))
              (flush-os! os))))
        (reflect-snapshots is cv))
      (catch java.io.IOException e
        (dispose! (to-root cv))
        (.printStackTrace e)))))
