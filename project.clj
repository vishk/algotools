(defproject algotools "0.1.0"
  :description "Widely-used graph and other problem solving algorithms."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.priority-map "LATEST"]]
  :aot [algotools.data.union-find])
