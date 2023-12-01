(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [com.taoensso/tufte "2.4.5"]
                 ]
  :plugins [[lein-ancient "1.0.0-RC3"]]
  :repl-options {:init-ns aoc.core})
