(ns bfs-clojure.core
  (:require [clojure.set :refer [difference]]))

(defn neighbors [position landscape]
  "Retrieve valid positions from landscape based on adjacent
   coords of provided position. Removing nils solves positions at
   edge of map."
  (filter (comp not nil?) (map landscape (position :adj))))

(defn find-value [value, landscape]
  "Find position containing provided value in the landscape.
   Using last to get the nested map out of the coord, map tuple
   that we get from filtering on a hashmap."
  (last (first (filter (fn [[coord pos]]
                   (= value (pos :value)))
                 landscape))))

(defn reject [fn coll] (filter (comp not fn) coll))

(defn construct-path [start finish originations]
  "Takes a map of position => orig-position (i.e. whence did i reach this position)
   and creates a vector of [x y] coords indicating all the positions in the path
   from start to finish"
  (loop [path [finish] curr finish]
    (if (originations curr)
      (recur (conj path (originations curr)) (originations curr))
      (reverse (map :coords path)))))

(defn queueable [current queued landscape]
  (let [neighbs (set (neighbors current landscape))
        queued (set (keys queued))]
    (difference  neighbs queued)))

(defn path [start finish landscape]
  "Search the landscape for a path from the start position to finish position. Returns path as sequence of [x y] coord pairs"
  (loop [curr start queue [] paths {start nil}]
    (if (= finish curr)
      (construct-path start finish paths)
      (let [queueable (queueable curr paths landscape)
            queue (into queue queueable)]
        (recur (first queue)
               (vec (rest queue))
               (into paths (for [n queueable] {n curr})))))))
