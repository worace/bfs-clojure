(ns bfs-clojure.core-test
  (:require [clojure.test :refer :all]
            [bfs-clojure.core :refer :all]))


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

;super simple landscape -- 2 lines with 1 space and 1 "#" in each line
(deftest parsing-a-landscape
  (testing "it parses lines of text into a landscape graph structure, returning the origin"
    ))

;; (deftest finding-start-position
;;   (testing "it finds coords for start-position in grid"
;;     (let [ls (parse-landscape (landscape-file "easiest.txt"))]
;;       (is (= [1 1] (start-pos ls))))))

; # -- [1,0]
;#S# -- [0,1 1,1(start) 2,1]
; #  -- [1,2]
(deftest finding-neighbors-coord
  (testing "it finds north,east,south,west neighbors"
    (is (= #{[1 0] [2 1] [1 2] [0 1]} (neighbors [1 1])))))


