(ns deathrider.client
  (:use seesaw.core))

(def ^:private SERVER_HOSTNAME "deathrider.ml")
(def ^:private SERVER_PORT deathrider.server/PORT)

(defn- new-canvas [paint-fn]
  (let [cv (canvas :background :balck :paint paint-fn)
        fr (frame :title "deathrider"
                  :width 500 :height 500
                  :content cv)]
    (-> fr show! invoke-later)
    cv))

