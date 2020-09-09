(ns
  ^{:author "Vish Kohli",
    :doc    "A set of widely-applicable (and fun!) graph theory algorithms and utilities.
   This includes algorithms for both unweighted and weighted graphs (where edge-costs are provided) as well as directed and undirected graphs, such as Breadth-First and Depth-First Searches, Shortest-path (Dijkstra), Minimum Spanning Trees, Pathfinding (A*) etc.
   In contrast with other languages, graphs that are usable by the functions in this namespace can be in flexible and 'natural' formats.  Most functions in this namespace work with 'Adjacency-list' graphs that are simply mapping of vertices to sequences of neighbors i.e. a graph like this:
   {:c (:b), :f (:g), :d (:e), :a (:b), :b (), :g (), :e ()}
or a weighted adjaceny-graph such as
   {:c '([:b 1] [:e 2]), :d '([:e 1]), :a '([:b 2] [:d 1]), :b '([:d 5]), :e '([:b 3]])}
   At the same time, one could start from as natural a format for unweighted graphs as just a sequences of edges (optionally accompanied by a set of vertices if some vertices are not connected). These edge-lists could be converted to the adjacency graph format or vice-versa using functions provided below.  Also see 'Example Usages' indicated in several function documentation below.

"}
  algotools.algos.graph
  ;(:use clojure.data.priority-map)
  (:use [clojure.data.priority-map :only [priority-map]])
    (:use algotools.data.union-find)
  (:import [algotools.data.union_find UnionFind]
           (java.util Map List)
           (clojure.lang IFn))
  (:gen-class
    :name algotools.algos.Graph
    :methods [^:static [scc_directed_graph [java.util.Map, clojure.lang.IFn] clojure.lang.PersistentHashSet]
              ^:static [neighbors [clojure.lang.Keyword, java.util.Map, java.util.Set, java.util.Map] java.util.List]
              ^:static [make_adjlist_graph [java.util.Set, java.lang.Boolean] java.util.Map]])
  )

;================= Graph building/conversion funcs

(defn make-adjlist-graph
  "Given a graph described by a sequence of edges, a directed? flag (true/false) and optionally a Set of vertices (in case some vertices have no incident edges), returns graph in adjacency list format (map of vertices to connected-neighbors). Example Usage:
   (make-adjlist-graph [[:c :b] [:f :g] [:d :e] [:a :b]] true )
  => {:c (:b), :f (:g), :d (:e), :a (:b), :b (), :g (), :e ()} "
 [edges directed? & {vertices :vertices :or {vertices (set (flatten (seq edges)))}}]
 {:pre [(or (true? directed?) (false? directed?)) (set? vertices)]}
 (letfn [(nbrmap-by [edges f g]
              (into {} (map (fn [[v eds]] [v (map g eds)]) 
                            (group-by f edges))))]
     (let [g0  (if directed?
                   (nbrmap-by edges first second)
                   (merge-with concat (nbrmap-by edges first second) 
                                      (nbrmap-by edges second first)))
           delta (remove (set (keys g0)) vertices)]
        (into g0 (map (fn [n] [n ()]) delta)))))


(defn get-edge-graph
  "Given a graph in adjacency-list format (map of vertices to neighbor-vertices), returns the set of all edges in the graph. The edge-set returned from this function plus optionally the set of vertices (in case any vertices have zero incident edges) is a succinct description of the graph that can be used with some graph functions in this package. Example Usage:
    (get-edge-graph {:g '(:f :e), :c '(:b :e), :f '(:g :e :d)}  false)   
   =>  #{[:c :b] [:f :g] [:f :d] [:g :e] [:f :e] [:c :e]}"
 [adjacencies directed]
    (let [G (reduce (fn [E [v nbrs]]
                      (into E (map (fn [n] 
                                     (if (or directed (not (E [n v]))) 
                                        [v n])) 
                                   nbrs))) #{} adjacencies)]
        (if directed G (set (filter identity G)))))


(defn to-unweighted-graph
"Given a weighted graph, drop edge-weights to make an unweighted one."
[wtd-g]
(into {} (map (fn [[v eds]] [v  (map first eds)])  wtd-g)))


(defn reverse-graph
  "Reverse a directed adjacency graph. Example Usage:
      (reverse-graph {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d)})
      => {:f [:d], :g [:f], :e (:c :d), :d [:a], :b (:a :c)} "  
 [g]
 (reduce (fn [R [v ns]]
             (merge-with concat R (into {} (map (fn [n] [n [v]])  ns))))
         {} g))

(defn reverse-wtd-graph
  "Reverse a directed adjacency graph. Example Usage:
      (reverse-wtd-graph {:c '([:b 1] [:e 2]), :a '([:b 3] [:d 4]), :d '([:e 6] [:f 5]), :f '([:g 7])})
      => {:f '([:d 5]), :g '([:f 7]), :e '([:c 2] [:d 6]), :d '([:a 4]), :b '([:a 3] [:c 1])} "
  [g]
  (let [temp-map (reduce (fn [R [v ns]]
            (merge-with concat R (into {} (map (fn [n] [(first n) [v (second n)]])  ns))))
          {} g),
        retval (into {} (map (fn [[k v]] [k, (map vec (partition 2 v))] )  temp-map))]
    retval) )

(defn neighbors
 "Given an adjacency-graph (map of adjacent vertices to each vertex), returns the list of neighbor-vertices of v so long as they are in the set of vertices (Un) provided as the third argument. If an additional argument :exclude is provided, then the behavior of Un is reversed i.e. neighbors are returned if they are not in Un (excluded from Un)."
 ([v g Un parents] ; parents is unused here but usable/needed in custom function (neighbors-fn) that replaces this function
 {:pre [(map? g) (set? Un)] }
 (filter Un (g v)))
 
 ([v g Un parents exclude]  ; parents is unused here but usable/needed in custom function (neighbors-fn) that replaces this function
 {:pre [(map? g) (set? Un)]}
 (remove Un (g v))))

;================= Graph Traversal and other unweighted-graph utilities
(def debug false)

(defn- dfs2
  "Helper function for dfs. The neighbors-fn function is used to determine valid neighbors of each vertex during the search. For more details, see documentation for dfs."
  [g, v, [U T P], & {neighbors-fn :neighbors-fn :or {neighbors-fn neighbors}}]
  (if debug (println "\nEntering DFS2 v= " v, " Unexplored = " U, ", Reverse-Finish-times = " T ", P = " P))
  (let [[Uu Tu Pu]
            (reduce (fn [[u t p] i]
                      (if debug (println "  In DFS2-reduce vtx= " i, " Unexplored = " u, ", Reverse-Finish-times = " t ", P = " p))
                      (if (u i)
                        (dfs2 g, i, [(disj u i) t (assoc p i v)], :neighbors-fn neighbors-fn)
                        [u t p]))
                    [U T P]
                    (neighbors-fn v g U P))]
    (if debug (println "\nLeaving DFS2 v= " v, " Unexplored = " Uu, ", Reverse-Finish-times = " (cons v Tu) ", P = " Pu))
    [Uu (cons v Tu) Pu] ))

(defn dfs 
 "Given an adjacency graph (g) and a start-vertex(node) traverses the graph in Depth-First-Search manner and returns a map comprising the following triad :
 - Unvisited-vertices (vertices that were unreachable) 
 - Traversed Vertices in decreasing order of Finish-times (the last vertex was the first terminal point in the Depth-first search, the penultimate the second terminal point and so on).
 - Map of each vertex to its Parent (vertex one hop before it). This map can be used to retrieve the depth-first-traversed-path to any vertex by using the 'path-to' function available in this namespace.
 You can optionally pass in a function to determine valid neighbors of each vertex during the search; otherwise 'neighbors' is used as the default neighbor-fn.
 Example Usage:
   (dfs {:c '(:b :e), :f '(:g), :d '(:e :f), :a '(:b :d), :b '(:d), :e '(:b :f :g) :g '()} :c :neighbor-fn neighbors)
  => {:unvisited #{:a}, :reverse-finish-order (:c :b :d :e :f :g), :parents {:g :f, :f :e, :e :d, :d :b, :b :c, :c nil}}"

 [g start & {neighbors-fn :neighbors-fn :or {neighbors-fn neighbors}}]
   (let [[U T P] 
           (dfs2 g start
                  [(disj (set (flatten (seq g))) start) () {start nil}] :neighbors-fn neighbors-fn)]
      {:unvisited U :reverse-finish-order T :parents P})

 )


(defn bfs 
  "Traverses an adjacency-graph (g) in Breadth-First-Search manner. Supports both directed and undirected graphs. Returns a set of vertices that could not be reached (if any) and a map of the parent of each vtx (the vertex one hop before it). This map can be used with the 'path-to' function elsewhere in this namespace to find the shortest path to any specific vertex from the start-vertex.
 You can optionally pass in a function to determine valid neighbors of each vertex during the search; otherwise 'neighbors' is used as the default neighbor-fn.\n
  Example Usage:
    (bfs {:g '(:f :e), :c '(:b :e), :f '(:g :e :d), :d '(:e :f :b :a), :a '(:b :d), :b '(:d :c :a :e), :e '(:b :f :g :d :c) } :c)
   => {:unvisited #{}, :parents {:c nil, :b :c, :e :c, :d :b, :a :b, :f :e, :g :e}}"
 [g start & {neighbors-fn :neighbors-fn :or {neighbors-fn neighbors}}]
 {:pre [(map? g)] }
 (loop [ q (conj clojure.lang.PersistentQueue/EMPTY start)
         U (disj (set (flatten (seq g))) start)
         P {start nil} ]
     (if-not (seq q)
         {:unvisited U :parents P}
         (let [v    (peek q)
               nbrs (neighbors-fn v, g, (set (keys P)), P, :exclude)]
           (recur (if (seq nbrs) (apply conj (pop q) nbrs) (pop q) ) 
                  (disj U v) 
                  (reduce #(into % {%2 v}) P nbrs))))))

(defn path-to 
"Get path to any vtx from start vertex used to generate the parent-map (typically returned by bfs/dfs). The parent-map must not end in a loop i.e. a vertex pointing to itself."
[v parent-map]
   (reverse (take-while identity (iterate parent-map v))))


(defn topological-sort 
"Given a DIRECTED ACYCLIC adjacency-graph, finds the ordering in which each vertex should be processed. It is assumed that the direction of each edge [u,v] indicates that u must be processed before v (for example u and v may be modeling scheduling dependencies). Do not use with UNDIRECTED graphs."
 [g]
 (loop [U (set (flatten (seq g))), T [] ] ;unvistd-set and Finish-ordered-verts
     (if-not (seq U)
         T
         (let [st (first U), 
              [u t _]  (dfs2 g st [(disj U st) T {st nil}])]
            (recur u t)))))


(defn scc-directed-graph 
"Returns a Set containing sequences of strongly connected components for a directed adjacency graph."
[g  & {neighbors-fn :neighbors-fn :or {neighbors-fn neighbors}}]
  (let [gr (reverse-graph g)
        _  (if debug (println "G = " g))
        _  (if debug (println "Grev = " gr))
        U0 (set (flatten (seq g)))
        FV (loop [U U0, T [] ] ;unvistd-set and Finish-ordered-verts
               (if-not (seq U)
                   T
                   (let [st (first U), 
                         [u t _]  (dfs2 gr, st, [(disj U st) T {st nil}], :neighbors-fn neighbors)] ;; for Grev use default neighbors
                     (if debug (println "After DFS on GRev from start " st, " Unexplored = " u, ", Reverse-Finish-times = " t))
                      (recur u t))))]
        ;;(alter-var-root #'debug (constantly false))
        (if debug (println "============== Phase2 ========="))
        (loop [V FV, C #{}]
           (if-not (seq V)
               C
               (let [st (first V)
                     [u t p] (dfs2 g, st, [(set (rest V)) [] {st nil}], :neighbors-fn neighbors-fn)
                     visited  (set (keys p))]
                 (if debug (println "After DFS on G from start " st, " Unexplored = " u, ", Reverse-Finish-times = " t))
                   (recur (remove visited V) (conj C t)))))))
;                  (recur (remove visited V) (conj C (set t))))))))

(defn cc-undirected-graph
 "Returns sets of connected components for an UNDIRECTED graph. For directed graphs, use scc-directed-graph."
[g & {neighbors-fn :neighbors-fn :or {neighbors-fn neighbors}}]
  (loop [U (set (flatten (seq g))), C #{} ]
    (if (seq U)
        (let [{u :unvisited pm :parents}  (bfs g, (first U), :neighbors-fn neighbors-fn)
              cv (set (keys pm))]
          (recur (apply disj U cv) (conj C cv)))
        C)))



;=================  Weighted Graph Functions/Algorithm-Tools

(defn make-wtd-adjlist-graph
  "Given a graph described by a map of edges and weights, a directed? flag (true/false) and optionally a Set of vertices (in case some vertices have no incident edges), returns graph in adjacency list format (map of vertices to connected-neighbors). Example Usage:
      (make-wtd-adjlist-graph {[:a :b] 4, [:c :e] 3, [:a :c] 1, [:a :d] 5} true)
     => {:a ([:c 1] [:b 4] [:d 5]), :c ([:e 3]), :b (), :d (), :e ()}"
 [wtd-edges directed? 
   & {vertices :vertices :or {vertices (set (flatten (keys wtd-edges)))}}]
 {:pre [(or (true? directed?) (false? directed?)) (set? vertices)]}
 (letfn [(nbrmap-by [wtd-edges f g]
              (into {} (map (fn [[v ed-wts]] [v (map (fn [[e w]] [(g e) w]) ed-wts)]) 
                            (group-by (fn [[e w]](f e)) wtd-edges))))]
     (let [g0  (if directed?
                   (nbrmap-by wtd-edges first second)
                   (merge-with concat (nbrmap-by wtd-edges first second) 
                                      (nbrmap-by wtd-edges second first)))
           delta (remove (set (keys g0)) vertices)]
       (into g0 (map (fn [n] [n ()]) delta)))))

(defn wtd-neighbors
 "Given a weighted adjacency-graph (map of vertices to neighbor-vertices with weights for connections), returns the list of neighbor-vertices of v with edge-weights, so long as they are in the set of vertices (Un) provided as the third argument. If an additional argument :exclude is provided, then the behavior of Un is reversed i.e. neighbors are returned if they are not in Un (excluded from Un)."
 ([v g Un]
 {:pre [(map? g) (set? Un)]}
 (filter #(Un (first %)) (g v)))
 
 ([v g Un exclude]
 {:pre [(map? g) (set? Un)]}
 (remove #(Un (first %)) (g v))))


(defn get-wtd-edge-graph
  "Given a weighted-graph in adjacency-list format (map of vertices to neighbor-vertices with weights for connections), returns a graph in form of a map of all edges and their respective weights. The edge-set returned from this function plus optionally the set of vertices (in case any vertices have zero incident edges) is a succinct description of the graph that can be used with some graph functions in this package."
 [adjacencies directed]
    (let [G (reduce (fn [E [v wtd-nbrs]]
                      (into E (map (fn [[n w]] 
                                     (if (or directed (not (E [n v]))) 
                                        [[v n] w])) 
                                   wtd-nbrs))) {} adjacencies)]
      (if directed G (into {} (filter identity G)))))


(defn shortest-path-dijk
"Efficient Dijkstra Implementation for a adjacency-graph with edge-costs. 
A goal vtx can be optionally indicated in which case a path to that vtx will be returned. 
Otherwise costs to each vtx and parent-ptrs are returned. The latter can be used with the path-to function to retrieve the shortest path to a vertex from the start."

  [g start & {goal :goal V :vertices :or {goal nil, V (set (keys g))}}]
  {:pre [(V start)]}       
  (let [; get nbors of vtx % (w/f-values) that are not already in done list
        ne  #(map (fn [[v c]]  [v (+ c %2)])
                  (wtd-neighbors % g %3 :exclude))]
    (loop [done #{}
           A  {}   ; path-costs to done vtces
           ; priority queue of [vtx and total cost (Au+Cuv) at v] 
           Q  (assoc (apply priority-map (mapcat (fn [v] [v Double/MAX_VALUE] )  V))
                     start 0)
           Pre {}]  ; Prev-vtx map to reconstruct path 
      (if (or (empty? Q) (= Double/MAX_VALUE (second (peek Q)))) 
        {:path-costs A, :prev-ptrs Pre}
        (let [ [v av] (peek Q) ]
          (if (= v goal)
            (reverse (take-while identity (iterate Pre v)))  ; return full path
            ; else add to A the path-cost of v, 
            ;      drop v from Q and 
            ;        update in Q all nbrs of v not already 'done'
            ;      add not-done nbrs to Pre with v as its prev-vtx                   
            (let [D2 (conj done v), vnbrs (ne v av D2),
                  [Q2 Pre2] (reduce (fn [[q p] [n an]] 
                                      (if (< an (q n)) 
                                        [(assoc q n an) (assoc p n v)] 
                                        [q p] )) 
                                    [(pop Q) Pre] vnbrs) ]
              (recur D2 (assoc A v av) Q2 Pre2 ))))))))

(defn MST-prim 
"Efficient implementation of Prim's algorithm to retrieve the Minimum Spanning Tree for a adjacency-graph with edge-costs. The MST-edges, Total-cost of MST-edges and the parent-pointers are returned. The last can be used with the path-to function to retrace the path to a vertex from the start."

[g start & {V :vertices :or {V (set (flatten (keys g)))}}]
{:pre [(V start)]}
        
(loop [done #{}
       T     #{} ; edges in tree
       Tcost 0
       ; priority queue of [vtx and edge cost to v] initialized
       Q     (assoc (apply priority-map (mapcat (fn [v] [v Double/MAX_VALUE] )  V))
                    start 0)
       Pre   {}]  ; Prev-vtx map to reconstruct path        
  (if (or (empty? Q) (= Double/MAX_VALUE (second (peek Q)))) 
    {:MST-edges T, :MST-cost Tcost, :prev-ptrs Pre}
    (let [ [v av] (peek Q) ]
    ; else drop v from Q and 
    ;      update in Q all nbrs of v not already 'done'
    ;      add not-done nbrs to Pre with v as prev-vtx                   
      (let [D2 (conj done v), vnbrs (wtd-neighbors v g D2 :exclude),
            [Q2 Pre2] (reduce (fn [[q p] [n wn]] 
                                (if (< wn (q n)) 
                                  [(assoc q n wn) (assoc p n v)] 
                                  [q p] )) 
                              [(pop Q) Pre] vnbrs)
            [w wc] (peek Q2) ]
        (recur D2 
               (if (and w (< wc Double/MAX_VALUE))
                   (conj T [(Pre2 w) w])
                   T)
               (+ Tcost av) 
               Q2 Pre2))))))


(defn MST-kruskal
"Minimum Spanning Tree by Kruskal's algorithm, given an adjacency graph with edge-costs."
  [g directed?]
  (let [egraph (get-wtd-edge-graph g directed?) 
        eds    (map first (sort-by second egraph)) 
        V  (set (flatten eds))
        uf (make-union-find V)
        MST (reduce (fn [T [x y]]
                          (if-not (same-comp? uf x y)
                                  (do (union uf x y)
                                      (conj T [x y]))
                                  T))
                    [] eds)]
      MST))


(defn pathfind-astar
  "Returns a path to be traversed in a grid of points (for example a maze) using the A* algorithm. Takes G (a vector of vectors representing the grid of points), a start and a goal (2-D vectors). An estimation-func (h) of 2 args(2-D vectors) may be optionally passed in using the key :est-func (default is euclidean dist to goal).  Any points that are never to be in path may be checked using a high-value indicated via optional arg :max, else  Double/MAX_VALUE will be used as default. Example Usage:
      (pathfind-astar [[1  1  1] 
                       [99 99 1] 
                       [1  1  1]]  [0 0] [2 0] :max 100 )
      => ([0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0])" 

  [G start goal 
             & {h :est-func M :max    :or 
                            {h (fn [a b] (Math/sqrt (reduce + (map #(let [d (- % %2)] (* d d) ) a b) )) )
                             M Double/MAX_VALUE }}]
  (let [W  (count (first G))
        H  (count G)
        nbr (memoize (fn [[i j] dn] 
                       (filter (fn [[x y]]
                                   (and (>= x 0) (>= y 0) (< x H) (< y W) 
                                        (not= M (get-in G [x y])) (not (dn [x y])) )) 
                               [[i (+ j 1)] [i (- j 1)] [(+ i 1) j] [(- i 1) j] ])))
        f   (memoize (fn [v g]  (+ (g v) (h v goal))))
        ; get path-costs g for nbors of %
        gn  (fn [u g dn]
              (map #(vector % (+ (get-in G %) (g u)) ) (nbr u dn)))
        ; get edges frm vtx % to its nbors (w/Fcalc) not already in done list
        ne  #(map (fn [x] [[% x] (f x %2)])
                  (nbr % %3)) ]   
    (loop [done #{start}
           A  (into {start 0} (gn start {start 0} done))  ; path-costs to known vtces
           ; FE =priority queue of each frontier edge (u,v) and total cost(g+h) at v 
           FE  (into (priority-map) (ne start A done))
           Pre { (-> FE peek first second) start } ]   ; prev-vtx mappg to reconstruct path
      (when-let [ [[u v] F] (peek FE)]
        (if (= v goal)
          (reverse (take-while identity (iterate (assoc Pre v u) v)))  ; return full path
          ; else add to A the path-costs of neighbors of v, 
          ;      drop u-v from FE and drop all edges ending in v 
          ;      add to FE edges from v that don't end in vtces already in 'done'
          ;      add to Pre vtx v with u as its prev-vtx
          (if (< F M)
            (let [D2 (conj done v), A2 (into A (gn v A D2))]
              (recur D2
                     A2
                     (into (priority-map) 
                           (concat (remove #(= v (-> % first second)) (pop FE))
                                   (ne v A2 D2)))
                     (assoc Pre v u)))))))))

(defn -neighbors [v g Un parents]
  (neighbors v g Un parents))

(defn -scc_directed_graph [^Map graph, ^clojure.lang.IFn nbrfunc] ;; nbrfunc will return neighbors with validation
  (scc-directed-graph graph :neighbors-fn nbrfunc) )

(defn -make_adjlist_graph [^Map graph, ^Boolean directed]
  (make-adjlist-graph graph, directed))