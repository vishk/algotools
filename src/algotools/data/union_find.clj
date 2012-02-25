(ns algotools.data.union-find)

; Union-Find Data structure and usage-functions. 
; Makes use of path-compression i.e. root-pointers are updated in findroot
; which is the main reason why this record uses refs for 
; pars (parent-pointers) and ranks.

(defrecord UnionFind [pars ranks])

(defn make-union-find 
"Given a set of vertices(nodes) create and initialize a Union-Find datastructure"
[V]
   (UnionFind. (ref (into {} (map (fn [v] [v v]) V)))
               (ref (into {} (map (fn [v] [v 1]) V)))))

(defn findroot
"Given a UnionFind record and a vertex(node) find the root-vertex of the 
component for that vertex and update the path to it for the given vertex"
[uf v] 
   (let [ps (deref (:pars uf))
         vp (ps v)
         r (if (= vp v) vp (findroot uf vp))]
       (if (and r (not= vp r))
         (dosync (alter (:pars uf) assoc v r)))
       r))

(defn union
"Given a UnionFind record and a pair of vertices(u and v) merge the two
components that u and v belong to." 
[uf u v]
   (let [r1 (findroot uf u), r2 (findroot uf v) ]
        (if (not= r1 r2) ; not in same component
           (let [rks (:ranks uf), ps (:pars uf), rk1 (@rks r1), rk2 (@rks r2)]
             (cond (= rk1 rk2 ) ; make r2 child of r1 and inc rank of r1
                     (dosync
                       (alter ps assoc r2 r1)
                       (alter rks assoc r1 (+ 1 rk1)))
                   (> rk1 rk2 ) ; make r2 child of r1, no need to change rank1
                     (dosync (alter ps assoc r2 r1))
                   (< rk1 rk2 ) ; make r1 child of r2, no need to change rank2
                     (dosync (alter ps assoc r1 r2)))))
        uf))

(defn same-comp? 
"Determine if u and v belong to the same component."
[uf u v]
    (= (findroot uf u) (findroot uf v)))

