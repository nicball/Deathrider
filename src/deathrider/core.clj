(ns deathrider.core
  (:gen-class)
  (:use [deathrider gameboard player point]))

(defn -main
  [& args]
  (let [gb (new-gameboard
             [(new-player 1 (new-point -10 0) :right)
              (new-player 2 (new-point 10 0) :left)])
        gb (nth (iterate #(step % {}) gb) 5)
        gb (step gb {1 :up})
        gb (nth (iterate #(step % {}) gb) 10)]
    (println gb)))
