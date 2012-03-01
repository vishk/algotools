# Algotools

A collection of widely-applicable graph and other problem solving algorithms.

These algorithms can be used to solve some cool problems such as navigating a Maze (with the pathfinding-astar algorithm) or finding how similar a pair of words are in terms of the number of operations needed to convert one to another (via the lev-dist function employing dynamic programming).

The graph algorithms include those that work with both unweighted and weighted graphs (where edge-costs are provided) as well as directed and undirected graphs, such as Breadth-First and Depth-First Searches, Shortest-path (Dijkstra), Minimum Spanning Trees, Pathfinding (A*) etc.

In contrast with these algorithms in other programming languages, graphs that are usable by the functions in this namespace can be in flexible and 'natural' formats.  Most functions in this namespace work with 'Adjacency-list' graphs that are simply mapping of vertices to sequences of neighbors i.e. a graph like this:  
   {:c (:b), :f (:g), :d (:e), :a (:b), :b (), :g (), :e ()}  

or a weighted adjacency-graph such as  

   {:c '([:b 1] [:e 2]), :d '([:e 1]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3]])}  

   At the same time, one could start from as natural a format for unweighted graphs as just a sequences of edges (optionally accompanied by a set of vertices if some vertices are not connected). These edge-lists could be converted to the adjacency graph format or vice-versa using conversion-functions provided.


## What's available

* Data Structures _(algotools.data)_:  
  - Union-Find with path-compression _(algotools.data.union-find)_  
      _make-union-find, findroot, union, same-comp?_  

* Graph based algorithms _(algotools.algos.graph)_:  
  - Graph creation/conversion funcs  
  - Graph Traversal and other unweighted-graph Algorithms  
     - Depth-First-Search _(dfs)_  
     - Breadth-First-Search _(bfs)_  
     - Topological-Sort (dependency tracking, usable for scheduling)  
     - Strongly Connected Components _(scc-directed-graph)_  
     - Connected Components for Undirected graph _(cc-undirected-graph)_  

   - Weighted Graph conversion/utility Functions  
   - Weighted Graph Algorithms  
     - Shortest-path by Dijkstra method  
     - Minimum Spanning Tree - Prim's algorithm (given start node)  
     - Minimum Spanning Tree - Kruskal's algorithm (no start node)  
     - PathFinding in grid - A* algorithm  

* Dynamic programming algorithms [work in progress] _(algotools.algos.dynpro)_:  
  - Levenshtein distance _(lev-dist)_  

## Installation

To use with Leiningen, add  

    [org.clojars.vishk/algotools "0.1.0"]  
to your project.clj dependencies.  

Or [get the jar](https://clojars.org/org.clojars.vishk/algotools) from clojars.org.

## Usage Examples 

[For more usage-guidance, see namespace and function-level documentation in the namespaces algotools.algos.graph, algotools.algos.dynpro and algotools.data.union-find.]

### Creation and manipulation

Create an unweighted directed graph (true => directed):  

    (make-adjlist-graph [[:c :b] [:f :g] [:d :e] [:a :b]] true )
    => {:c (:b), :f (:g), :d (:e), :a (:b), :b (), :g (), :e ()}

Get edges from an unweighted graph:  

    (get-edge-graph {:g '(:f :e), :c '(:b :e), :f '(:g :e :d)}  false)  
    => #{[:c :b] [:f :g] [:f :d] [:g :e] [:f :e] [:c :e]}

Reverse a graph:  

    (reverse-graph {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d)})
    => {:f [:d], :g [:f], :e (:c :d), :d [:a], :b (:a :c)}

Create a weighted graph (with edge-weights):  

    (make-wtd-adjlist-graph {[:a :b] 4, [:c :e] 3, [:a :c] 1, [:a :d] 5} true)
    => {:a ([:c 1] [:b 4] [:d 5]), :c ([:e 3]), :b (), :d (), :e ()}

Get edges and edge-weights for a weighted graph:  

    (get-wtd-edge-graph {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), 
                         :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), 
                         :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), :c '([:e 3] [:a 1])} 
                        false)
    => {[:g :d] 7, [:f :g] 3, [:d :e] 2, [:a :c] 1, [:b :g] 5, [:a :b] 4, [:f :e] 1, [:c :e] 3, [:a :d] 5}

### Traversing graphs

Traversal in _Depth-First-Search_ style (start-node is :c):  

    (dfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} 
         :c) 
    => {:unvisited #{:a}, :reverse-finish-order (:c :b :d :e :f :g), :parents {:g :f, :f :e, :e :d, :d :b, :b :c, :c nil}}

Get path to node :g from results of DFS traversal:  
    
    (def dfs_results 
         (dfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} :c)
    (path-to :g (:parents dfs_results))
    => (:c :b :d :e :f :g)

Get path to node :e from results of DFS traversal:  

    (path-to :e (:parents dfs_results))
    => (:c :b :d :e)

Traverse a graph in _Breadth-First-Search_ style (start-node is :c):  

    (bfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} 
         :c)
    => {:unvisited #{:a}, :parents {:c nil, :b :c, :e :c, :d :b, :f :e, :g :e}}


Get path to node :g from results of BFS traversal:  

    (def bfs_results 
         (bfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} :c)
    (path-to :g (:parents bfs_results))
    => (:c :e :g)

### Shortest-path, Minimum Spanning Tree

Get shortest-path from :a to :f, and :a to :g (use Dijkstra's algo):  

    (def spaths 
         (shortest-path-dijk {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), 
                              :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), 
                              :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), 
                              :c '([:e 3] [:a 1])} 
                             :a))
    (path-to :f (:prev-ptrs spaths))
    => (:a :c :e :f)
    (path-to :g (:prev-ptrs spaths))
    => (:a :c :e :f :g)

Directly get shortest path (Dijkstra) to goal :f from start-node :a  

    (shortest-path-dijk {:g '([:d 7] [:f 3] [:b 5]), :e '([:d 2] [:f 1] [:c 3]), 
                         :d '([:g 7] [:e 2] [:a 5]), :f '([:g 3] [:e 1]), 
                         :a '([:c 1] [:b 4] [:d 5]), :b '([:g 5] [:a 4]), 
                         :c '([:e 3] [:a 1])} 
                         :a :goal :f)
    => (:a :c :e :f)

Get Minimum Spanning Tree of edges using Kruskal's algo and UnionFind:  

    (MST-kruskal {:c '([:b 1] [:e 2]), :f '([:g 3]), :d '([:e 1] [:f 4]), 
                  :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3] [:f 4] [:g 5])} true)
    =>[[:c :b] [:d :e] [:a :d] [:a :b] [:f :g] [:e :f]]

### Dynamic programming  

Get Levenshtein distance (# of ops to mutually convert) between 2 strings, assuming insertion, deletion and substitution of character are legal ops:  

    (lev-dist "hello" "yellow")
    => 2
    (lev-dist "TCTAATTTTCTG" "TCAACCCTACCAT")
    => 8

### Other  
Given a graph of precedences, determine traversal-order to resolve dependencies:  

    ;Dependencies:-  :c must go before :b and :e, :f before :g, and so on.
    (def g {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d :e), :e '(:g) :g '()})
    (topological-sort g)
    => (:c :a :b :d :f :e :g)

Get connected components of an undirected graph (sets of mutually reachable nodes):  

    (cc-undirected-graph {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), 
                          :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), 
                          :e '(:b :f :g :d :c) :h '(:i) :i '(:h)})
    => [#{:a :c :b :f :g :d :e} #{:i :h}]

Get strongly-connected components (all nodes in each comp are mutually reachable):  

    (scc-directed-graph {3 [9], 9 [6], 6 [3], 1 [6 7], 7 [4], 4 [1], 2 [4 5], 5 [8], 8 [2]})
    => [#{3 6 9} #{1 4 7} #{2 5 8}]

## Dependencies

This project uses clojure.data.priority-map.
 
## License

Copyright (c) 2012 Vish Kohli

Distributed under the Eclipse Public License, the same as Clojure.
