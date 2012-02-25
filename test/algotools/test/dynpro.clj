(ns algotools.test.dynpro
 (:use clojure.test)
 (:require [algotools.algos.dynpro :as dp]))

(deftest test-lev-dist
  (is (= 2 (dp/lev-dist "hello" "yellow")))
  (is (= 1 (dp/lev-dist "misty" "musty")))
  (is (= 6 (dp/lev-dist "volatile" "vulgar")))
  (is (= 3 (dp/lev-dist "sugar" "singer")))
  (is (= 0 (dp/lev-dist "misty" "misty")))
  )
