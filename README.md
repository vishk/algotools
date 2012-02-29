# Algotools

A collection of widely-applicable graph and other problem solving algorithms.

These algorithms can be used to solve some cool problems such as navigating a Maze (with the pathfinding-astar algorithm) or finding how similar a pair of words are in terms of the number of operations needed to convert one to another (via the lev-dist function employing dynamic programming).

The graph algorithms include those that work with both unweighted and weighted graphs (where edge-costs are provided) as well as directed and undirected graphs, such as Breadth-First and Depth-First Searches, Shortest-path (Dijkstra), Minimum Spanning Trees, Pathfinding (A*) etc.

In contrast with these algorithms in other programming languages, graphs that are usable by the functions in this namespace can be in flexible and 'natural' formats.  Most functions in this namespace work with 'Adjacency-list' graphs that are simply mapping of vertices to sequences of neighbors i.e. a graph like this:  
   {:c (:b), :f (:g), :d (:e), :a (:b), :b (), :g (), :e ()}  

or a weighted adjacency-graph such as  

   {:c '([:b 1] [:e 2]), :d '([:e 1]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3]])}  

   At the same time, one could start from as natural a format for unweighted graphs as just a sequences of edges (optionally accompanied by a set of vertices if some vertices are not connected). These edge-lists could be converted to the adjacency graph format or vice-versa using conversion-functions provided.  Also see 'Example Usages' indicated in function documentation.


## What's available

[For specific usage-guidance, see namespace and function-level documentation in the namespaces algotools.algos.graph, algotools.algos.dynpro and algotools.data.union-find.]

* Data Structures _(algotools.data)_:  
  - Union-Find _(algotools.data.union-find)_  
      _make-union-find, findroot, union, same-comp?_  

* Graph based algorithms _(algotools.algos.graph)_:  
  - Graph building/conversion funcs  
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

## Dependencies

This project uses clojure.data.priority-map.
 
## License

Copyright (c) 2012 Vish Kohli

Distributed under the Eclipse Public License, the same as Clojure.
