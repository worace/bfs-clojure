(ns bfs-clojure.landscape-test
  (:require [bfs-clojure.landscape :refer :all]
            [clojure.test :refer :all]))


(deftest finding-adj-coords
  (testing "it finds north,east,south,west neighbors"
    (is (= #{[1 0] [2 1] [1 2] [0 1]} (adj-coords [1 1])))))

(deftest reading-a-landscape-file
  (testing "it reads contents of landscape file from resources"
    (is (= "# \n #") (landscape-file "2by2.txt"))))

(deftest coords-of-rows-test
  (testing "turns seq of row strings into seq of coord, value tuples"
    (let [expected [[[0 0] "a"] [[1 0] "b"] [[0 1] "c"] [[1 1] "d"]]]
      (is (= expected
             (coords ["ab" "cd"]))))))

(deftest coord-map-test
  (testing "turns seq of coord, value tuples into coord -> value map"
    (let [coords [[[0 0] "a"] [[1 0] "b"]]
          mapped (coord-map coords)]
      (is (= "a" (mapped [0 0])))
      (is (= "b" (mapped [1 0]))))))

(deftest parsing-a-landscape
  (testing "it parses lines of text into a landscape graph structure, returning the origin"
    (let [ls (parse-landscape "ab\ncd")]
      (is (= 4 (count ls)))
      (is (= #{[0 0] [1 0] [0 1] [1 1]} (into #{} (keys ls))))
      (is (= "d" ((ls [1 1]) :value)))
      )))

