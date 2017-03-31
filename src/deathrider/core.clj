(ns deathrider.core
  (:gen-class)
  (:use [deathrider server]))

(defn -main
  [& args]
  (start-server))
