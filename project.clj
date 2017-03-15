(defproject deathrider "0.1.0-SNAPSHOT"
  :description "The deathrider game."
  :url "https://www.cctv.com"
  :license {:name "Eclipse Public License"
            :distribution :repo
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [seesaw "1.4.6-SNAPSHOT"]]
  :main ^:skip-aot deathrider.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
