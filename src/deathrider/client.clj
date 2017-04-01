(ns deathrider.client
  (:use [seesaw core graphics]
        [deathrider config gameboard player point]))

(def ^:private SERVER_HOSTNAME "deathrider.ml")
(def ^:private CANVAS_SIZE 500)
(def ^:private UNIT_SIZE (/ CANVAS_SIZE (inc GAMEBOARD_SIZE)))

(defn- new-canvas []
  (let [cv (canvas :background :black :paint nil
                   :size [CANVAS_SIZE :by CANVAS_SIZE])
        fr (frame :title "deathrider"
                  :content cv
                  :on-close :exit)]
    (-> fr pack! show! invoke-later)
    cv))

(def ^:private color-map
  [:lime :red :aqua :yellow :blue :fuchsia :teal :green])

(defn- paint [cv f]
  (config! cv :paint f))

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

(defn start-client []
  (let [cv (new-canvas)
        gb (new-gameboard [(new-player 0 (new-point -5 0) :right)
                           (new-player 1 (new-point 5 0) :left)]
                          GAMEBOARD_SIZE
                          GAMEBOARD_SIZE)
        gb (last (take 4 (iterate #(step % {}) gb)))]
    (paint cv (fn [_ g]
                (paint-background g)
                (doall (map (partial paint-player g)
                            (gameboard-players gb)))))))
