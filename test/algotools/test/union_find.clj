(ns algotools.test.union-find
 (:use clojure.test)
 (:use algotools.data.union-find)
 (:import [algotools.data.union_find UnionFind]))

(deftest test-make-union-find
  (def u (make-union-find #{:a :b :c :d :e :f :g}))
  (is (= {:a :a, :c :c, :b :b, :f :f, :g :g, :d :d, :e :e}
         (deref (:pars u))))
  (is (= {:a 1, :c 1, :b 1, :f 1, :g 1, :d 1, :e 1}
         (deref (:ranks u)))
      ))

(deftest test-findroot-union
  (def u (make-union-find #{:a :b :c :d :e :f :g}))
  (union u :a :b)
  (is (= {:a :a, :c :c, :b :a, :f :f, :g :g, :d :d, :e :e}
         (deref (:pars u))))
  (is (= {:a 2, :c 1, :b 1, :f 1, :g 1, :d 1, :e 1}
         (deref (:ranks u))))
  (is (= :a (findroot u :b) ))
  
  (union u :c :d)
  (is (= {:a :a, :c :c, :b :a, :f :f, :g :g, :d :c, :e :e}
         (deref (:pars u))))
  (is (= {:a 2, :c 2, :b 1, :f 1, :g 1, :d 1, :e 1}
         (deref (:ranks u))))
  (is (= :c (findroot u :d) ))

  (union u :b :d)
  (is (= {:a :a, :c :a, :b :a, :f :f, :g :g, :d :c, :e :e}
         (deref (:pars u))))
  (is (= {:a 3, :c 2, :b 1, :f 1, :g 1, :d 1, :e 1}
         (deref (:ranks u))))
  (is (= :a (findroot u :d) (findroot u :c) (findroot u :b) (findroot u :a)))
  
  ; path compression- after findroot on :d, it should point directly to :a
  (is (= {:a :a, :c :a, :b :a, :f :f, :g :g, :d :a, :e :e}
         (deref (:pars u))))
  )

(deftest test-same-comp
  (def u (make-union-find #{:a :b :c :d :e :f :g}))
  (is (= false (same-comp? u :a :b)))

  (union u :a :b)
  (is (= true (same-comp? u :a :b)))

  (union u :c :d)
  (is (= false (same-comp? u :d :b)))
  
  (union u :b :d)
  (is (= true (same-comp? u :d :b)))
  )
