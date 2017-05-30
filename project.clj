(defproject combinatorica "0.1.0-SNAPSHOT"
  :description "Faster implementation of combinatorial computation compatible with math.combinatorics"
  :url "https://github.com/athos/combinatorica"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies
                   [[criterium "0.4.4"]
                    [org.clojure/math.combinatorics "0.1.4"]]}})
