(defproject org.clojars.vishk/algotools "0.1.0"
  :description "Widely-applicable graph and other problem solving algorithms."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.priority-map "LATEST"]]
  :aot [algotools.data.union-find]
  :dev-dependencies [[lein-clojars "0.7.0" :exclusions [org.clojure/clojure]]])
