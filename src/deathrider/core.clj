(ns deathrider.core
  (:gen-class)
  (:use [deathrider server client]))

(defn -main
  [& args]
  (if (seq args)
    (start-server)
    (start-client)))
