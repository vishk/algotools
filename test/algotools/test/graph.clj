(ns algotools.test.graph
 (:use clojure.test)
 (:use algotools.data.union-find)
 ; (:import [algotools.data.union_find UnionFind] )
 (:require [algotools.algos.graph :as g]))

; Graph creation/conversion and unweighted graph tests

(defn- same-adjlist-graphs?
  "Compares the two graph-maps without regard to order in their values (if they are collections)"
  [{keys1 :keys :as map1}, {keys2 :keys :as map2} ]
  (letfn [(values-match? [key] ; do values from both maps match for this key?
            (let [val1 (map1 key), val2 (map2 key)]
              (if (coll? val1)
                (= (set val1) (set val2)) ; convert to sets as order may not match
                (= val1 val2))))]

    (and (= keys1 keys2)
         (every? values-match? keys1)) ))

(defn- same-undirected-edge-graphs?
  "Compares the two graph-sets without regard to order in their vertices"
  [graph1 graph2]
  (let [set-of-sets1 (set (map set graph1)),
        set-of-sets2 (set (map set graph2))]
    (= set-of-sets1 set-of-sets2))
  )

(deftest test-making-adjlist-graph
  (is (same-adjlist-graphs? {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()}
                            (g/make-adjlist-graph
       #{[:c :b] [:f :g] [:d :e] [:a :b] [:b :d] [:c :e] [:a :d] [:e :b] [:e :f] [:d :f] [:e :g]} true)) )
  (is (same-adjlist-graphs? {:e '(:d :g :f :b :c), :c '(:b :e), :f '(:g :d :e), :d '(:e :f :b :a), :g '(:e :f), :a '(:b :d), :b '(:d :e :c :a)}
                            (g/make-adjlist-graph
          #{[:c :b] [:f :g] [:d :e] [:f :d] [:g :e] [:a :b] [:f :e] [:b :d] [:b :e] [:c :e] [:a :d]} false)))

  (is (same-adjlist-graphs? {:a '(:b :d), :c '(:b :e), :b '(:d :c :a :e), :f '(:g :e :d), :g '(:f :e), :d '(:e :f :b :a), :e '(:b :f :g :d :c), :i '(), :h '()}
                            (g/make-adjlist-graph
          #{[:c :b] [:f :g] [:d :e] [:a :b] [:b :d] [:c :e] [:a :d] [:e :b] [:e :f] [:d :f] [:e :g]} false :vertices #{:a :b :c :d :e :f :g :h :i})))
  )


(deftest test-get-edges-from-adjlist-graph
  ;undirected graph
  (is (same-undirected-edge-graphs? #{[:c :b] [:f :g] [:d :e] [:f :d] [:g :e] [:a :b] [:f :e] [:b :d] [:b :e] [:c :e] [:a :d]}
         (g/get-edge-graph
          {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c)} false)))
  ; directed graph
  (is (= #{[:c :b] [:f :g] [:d :e] [:a :b] [:b :d] [:c :e] [:a :d] [:e :b] [:e :f] [:d :f] [:e :g]}
         (g/get-edge-graph
          {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g)} true)))
  )


(deftest test-conversion-wtd-graph-to-unweighted
  (is (same-adjlist-graphs? {:a '(:b :d), :c '(:b :e), :b '(:d), :f '(:g), :d '(:e :f), :e '(:b :f :g)}
                            (g/to-unweighted-graph {:c '([:b 1] [:e 2]), :f '([:g 3]), :d '([:e 1] [:f 4]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3] [:f 4] [:g 5])})))
  )


(deftest test-reversing-adlist-graph
  (is (same-adjlist-graphs? {:d '(:a :b), :f '(:d :e), :g '(:f :e), :e '(:c :d), :b '(:c :a :e)}
                            (g/reverse-graph {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g ()})))
  (is (same-adjlist-graphs? {2 [8], 3 [6], 8 [5], 1 [4], 9 [3], 5 [2], 4 [2 7], 7 [1], 6 [1 9]}
                            (g/reverse-graph {3 [9], 9 [6], 6 [3], 1 [6 7], 7 [4], 4 [1], 2 [4 5], 5 [8], 8 [2]})))
  )

(deftest test-dfs
  ;Undirected adj-graph
  (is (= {:unvisited #{} :reverse-finish-order '(:c :b :d :a :e :f :g) :parents {:a :d, :g :f, :f :e, :e :d, :d :b, :b :c, :c nil}}
         (g/dfs {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c)} :c)))

  ;Directed adj-graph
  (is (= {:unvisited #{:a} :reverse-finish-order '(:c :b :d :e :f :g) :parents {:g :f, :f :e, :e :d, :d :b, :b :c, :c nil}}
         (g/dfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} :c)))

  ;Directed adj-graph with validation exclude :f
  (is (= {:unvisited #{:a :f} :reverse-finish-order '(:c :b :d :e :g) :parents {:g :e, :e :d, :d :b, :b :c, :c nil}}
         (g/dfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()},
                :c, :neighbors-fn (comp #(filter (complement #{:f}) %), g/neighbors))))

  )

(deftest test-bfs
  (is (= {:unvisited #{:i :h}, :parents {:c nil, :b :c, :e :c, :d :b, :a :b, :f :e, :g :e}}
         (g/bfs {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c) :h '(:i)} :c)))

  (is (= {:unvisited #{}, :parents {:c nil, :b :c, :e :c, :d :b, :a :b, :f :e, :g :e}}
         (g/bfs {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c)} :c)))
  
  (is (= {:unvisited #{:a}, :parents {:c nil, :b :c, :e :c, :d :b, :f :e, :g :e}}
         (g/bfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} :c)))

  )

(deftest test-path-reconstruction
  (is (= '(:c :e :f)
         (g/path-to :f {:c nil, :b :c, :e :c, :d :b, :a :b, :f :e, :g :e})))

  (is (= '(:c :e :g)
         (g/path-to :g {:c nil, :b :c, :e :c, :d :b, :a :b, :f :e, :g :e})))
  
  )

(deftest test-topological-sort
  (let [ts1 (g/topological-sort
              {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d :e), :e '(:g) :g '()})]
    (is (or (= '(:c :a :b :d :f :e :g) ts1) (= '(:a :c :b :d :f :e :g) ts1)) ))

  (is (= '(:g :a :b :c :f :e :d)
         (g/topological-sort {:g '(:a :f), :a '(:b :c), :b '(:c :d), :c '(:e :f), :e '(:d), :f '(:e)})
         ))
  )

(defn neighbors-with-validation [v g Un p]
  {:pre [(map? g) (set? Un)] :post [(or (if nil (println "Vtx = ", v, ", Valid-Neighbors = ", %)) true)]}
  (let [nbrs (g/neighbors v g Un p)]
    (filter (complement #(#{v %} 5)) nbrs)) ; allow if neither the neighbor nor v is 5
  )

(deftest test-strongly-conn-comps-dir-graph
  (is (= #{'(:c) '(:g) '(:f) '(:a) '(:e :b :d)}
         (g/scc-directed-graph
          {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} )))
  (is (= #{ '(6 3 9) '(2 5 8) '(7 4 1)}
         (g/scc-directed-graph
          {3 [9], 9 [6], 6 [3], 1 [6 7], 7 [4], 4 [1], 2 [4 5], 5 [8], 8 [2]})))

  (is (= #{ '(6 3 9), '(7 4 1) '(2) '(5) '(8) }
         (g/scc-directed-graph
           {3 [9], 9 [6], 6 [3], 1 [6 7], 7 [4], 4 [1], 2 [4 5], 5 [8], 8 [2]}, :neighbors-fn neighbors-with-validation)))
  )

(deftest test-conn-comps-undir-graph
  (is (= #{ #{:a :c :b :f :g :d :e} #{:i :h} }
         (g/cc-undirected-graph
          {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c) :h '(:i) :i '(:h)}))))


; Weighted graph tests

(deftest test-making-wtd-adjlist-graph
  ; directed graph
  (is (same-adjlist-graphs? {:e '([:d 2] [:f 1]), :d '([:g 7]), :f '([:g 3]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5]), :c '([:e 3]), :g '()}
                            (g/make-wtd-adjlist-graph {[:a :b] 4, [:b :g] 5, [:c :e] 3, [:a :c] 1, [:a :d] 5, [:e :d] 2, [:d :g] 7, [:e :f] 1, [:f :g] 3} true)))

  ; undirected-graph
  (is (same-adjlist-graphs? {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])}
                            (g/make-wtd-adjlist-graph {[:a :b] 4, [:b :g] 5, [:c :e] 3, [:a :c] 1, [:a :d] 5, [:e :d] 2, [:d :g] 7, [:e :f] 1, [:f :g] 3} false)))
  )

(deftest test-reversing-wtd-graph
  (is (same-adjlist-graphs? {:e '([:c 3] ), :f '([:e 1]), :g '([:f 3] [:d 7] [:b 5]), :b '([:a 4]), :d '([:a 5] [:e 2]),  :c '([:a 1]), :a '()}
                            (g/reverse-wtd-graph {:e '([:d 2] [:f 1]), :d '([:g 7]), :f '([:g 3]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5]), :c '([:e 3]), :g '()})))

  (is (same-adjlist-graphs? {:f '([:d 5]), :g '([:f 7]), :e '([:c 2] [:d 6]), :d '([:a 4]), :b '([:a 3] [:c 1])}
        (g/reverse-wtd-graph {:c '([:b 1] [:e 2]), :a '([:b 3] [:d 4]), :d '([:e 6] [:f 5]), :f '([:g 7])})))
  )

(deftest test-getting-edges-wtd-graph
  (is (same-adjlist-graphs? {[:g :d] 7, [:f :g] 3, [:d :e] 2, [:a :c] 1, [:b :g] 5, [:a :b] 4, [:f :e] 1, [:c :e] 3, [:a :d] 5}
                            (g/get-wtd-edge-graph {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} false)))

  (is (= {[:c :b] 1, [:f :g] 3, [:d :e] 1, [:a :b] 2, [:b :d] 5, [:c :e] 2, [:a :d] 1, [:e :b] 3, [:e :f] 4, [:d :f] 4, [:e :g] 5}
         (g/get-wtd-edge-graph {:c '([:b 1] [:e 2]), :f '([:g 3]), :d '([:e 1] [:f 4]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3] [:f 4] [:g 5])} true)))
  )

(deftest test-getting-selected-wtd-neighbors
  (def Un #{:a :c :b :e})
  ; get only ones in Un
  (is (= '([:c 1] [:b 4])
         (g/wtd-neighbors :a {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} Un)))

  (is (= '([:b 5])
         (g/wtd-neighbors :g {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} Un)))

  ; get ones not in Un
  (is (= '([:d 7] [:f 3])
         (g/wtd-neighbors :g {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} Un :exclude)))
  )

(deftest test-shortest-path-dijkstra
  ; shortest path from a start-node to all nodes/vertices
  (is (= {:path-costs {:g 8, :d 5, :f 5, :e 4, :b 4, :c 1, :a 0}, :prev-ptrs {:f :e, :g :f, :e :c, :d :a, :b :a, :c :a}}
         (g/shortest-path-dijk {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} :a)))

  ; shortest-path from a start-node to a specific goal node/vtx
  (is (= '(:a :c :e :f)
         (g/shortest-path-dijk {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} :a :goal :f)))

  )

(deftest test-minimum-spanning-tree-by-prim
  ; Undirected-graph:
  (is (= {:MST-edges #{[:e :d] [:f :g] [:a :c] [:a :b] [:c :e] [:e :f]}, :MST-cost 14, :prev-ptrs {:g :f, :f :e, :e :c, :d :e, :b :a, :c :a}}
         (g/MST-prim {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} :a)))

  ; directed-graph
  (is (= {:MST-edges #{[:c :b] [:f :g] [:b :d] [:c :e] [:e :f]}, :MST-cost 15, :prev-ptrs {:g :f, :f :e, :d :b, :e :c, :b :c}}
         (g/MST-prim {:c '([:b 1] [:e 2]), :f '([:g 3]), :d '([:e 1] [:f 4]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3] [:f 4] [:g 5]), :g '()} :c)))

  )


(deftest test-minimum-spanning-tree-by-kruskal
  ; Undirected-graph:
  (is (= [[:c :b] [:d :e] [:a :d] [:a :b] [:f :g] [:e :f]]
         (g/MST-kruskal {:c '([:b 1] [:e 2]), :f '([:g 3]), :d '([:e 1] [:f 4]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3] [:f 4] [:g 5])} true)))

  ; directed-graph
  (is (= [[:a :c] [:f :e] [:d :e] [:f :g] [:c :e] [:a :b]]
         (g/MST-kruskal {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} false)))
  )

(deftest test-pathfinding-astar
  (is (= '([0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0])
         (g/pathfind-astar [[1  1  1] 
                          [99 99 1] 
                          [1  1  1]]  [0 0] [2 0] :max 100 )))

  (is = (= '([0 1] [0 2] [0 3] [0 4] [1 4] [2 4] [2 3] [2 2] [3 2] [4 2] [4 1] [4 0])
           (g/pathfind-astar [[6  1   1  2  1] 
                            [50 99 99 99  1] 
                            [1  99  1  1  1]
                            [1  99  1 99  1]
                            [1   2  1 99  1]] [0 1] [4 0] :max 100)))
  (is (= '([0 1] [0 0] [1 0] [2 0] [3 0] [4 0])
         (g/pathfind-astar [[6  1   1  2  1] 
                          [1  99 99 99  1] 
                          [1  99  1  1  1]
                          [1  99  1 99  1]
                          [1   2  1 99  1]] [0 1] [4 0] :max 100)))
  )


  