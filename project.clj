(defproject org.clojars.vishk/algotools "0.2.0"
  :min-lein-version "2.0.0"
  :description "Widely-applicable graph and other problem solving algorithms."
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.priority-map "0.0.10"]]
  :aot [algotools.data.union-find
        algotools.algos.graph]
  :dev-dependencies [[lein-clojars "0.7.0" :exclusions [org.clojure/clojure]]])
