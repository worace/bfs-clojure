(ns bfs-clojure.core-test
  (:require [clojure.test :refer :all]
            [bfs-clojure.landscape :as ls]
            [bfs-clojure.core :refer :all]))

(def abcd (ls/parse-landscape "ab\ncd"))
(def easiest (ls/from-file "easiest.txt"))

(deftest finding-neighbors
  (testing "it finds neighbor positions that are present in the map"
      (is (= 2 (count (neighbors (abcd [0 0]) abcd))))))

(deftest finding-value-position
  (testing "finds a position containing specified value"
    (is (= "c" ((find-value "c" abcd) :value)))
      (is (= "S" ((find-value "S" easiest) :value)))
      (is (nil? (find-value "A" easiest)))))

(deftest neighbors-of-start
  (let [ls easiest
        start (find-value "S" ls)]
    (is (contains? (set (map :value (neighbors start ls)) ) "F"))))

(deftest searching-simple-landscape
  (testing "finds sequential path from start position to desired target"
    (let [ls (ls/from-file "easiest.txt")
          start (find-value "S" ls)
          finish (find-value "F" ls)]
      ;;full path including start and finish
      (is (= [[1 1] [2 1]] (path start finish ls))))))
