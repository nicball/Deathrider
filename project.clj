(defproject deathrider "0.1.0-SNAPSHOT"
  :description "The deathrider game."
  :url "https://www.cctv.com"
  :license {:name "Eclipse Public License"
            :distribution :repo
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [seesaw "1.4.5"]
                 [com.taoensso/nippy "2.13.0"]
                 [org.clojure/core.async "0.3.442"]]
  :main ^:skip-aot deathrider.core
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}})
