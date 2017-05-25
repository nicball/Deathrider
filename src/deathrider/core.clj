(ns deathrider.core
  (:gen-class)
  (:use [deathrider server client]))

(defn -main
  [& args]
  (if (seq args)
    (start-client (first args))
    (start-server)))
