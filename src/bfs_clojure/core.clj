(ns bfs-clojure.core)

(defn coords [rows]
  "take seq of strings representing map rows
   and parse them into sequence of coordinate, position-value
   tuples. e.g (([0 0] 'a') ([1 0] 'b'))"
  (mapcat identity
          (map-indexed (fn [y row]
                         (map-indexed
                          (fn [x char] [[x y] (str char)])
                          row))
                       rows)))

(defn coord-map [coords]
  "take seq of coordinate-value tuples and return
   map of [x y] -> 'value' coords. Use mapcat identity as
   a 1-level flatten"
  (apply hash-map (mapcat identity coords)))


(def shifts {:north [-1 0] :west [0 -1] :east [1 0] :south [0 1]})
(defn adj-coords [coords]
  "Turn x,y coord into set of shifted neighbor coords
   e.g [0 0] -> #{[1 0] [0 1] [-1 0] [0 -1]}"
  (into #{} (map vec
                 (for [shift (vals shifts)]
                   (map #(apply + %) (map vector coords shift))))))

(defn neighbors [position landscape]
  "Retrieve valid positions from landscape based on adjacent
   coords of provided position. Removing nils solves positions at
   edge of map."
  (filter (comp not nil?) (map landscape (position :adj))))

(defn graphed [coord-map]
  "Turn map of coord -> value to map with enriched values
   containing :value and :adj keys. E.G:
   {[0 0] 'a'} -> {[0 0] {:value 'a' :adj #{[1 0] [0 1]...}}}"
  (reduce (fn [cm [coords value]]
            (assoc cm coords (into {:value value} {:adj (adj-coords coords) :coords coords})))
          {}
          coord-map))

(defn landscape-file [filename]
  "read string content of provided filename"
  (slurp (.getFile (clojure.java.io/resource filename))))
(defn lines [str] (clojure.string/split str #"\n"))

(defn parse-landscape [landscape-string]
  "Parse newline-delimited string representing a landscape
   into map of [x y] coord to maps of positions containing
   :value and :adj(acent coords)"
  (graphed (coord-map (coords (lines landscape-string)))))

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

(defn path [start finish landscape]
  "Search the landscape for a path from the start position to finish position. Returns path as sequence of [x y] coord pairs"
  (loop [curr start queue [] paths {start nil}]
    (if (= finish curr)
      (construct-path start finish paths)
      (let [queueable (reject (partial contains? paths) (neighbors curr landscape))
            queue (into queue queueable)]
        (recur (first queue)
               (vec (rest queue))
               (into paths (for [n queueable] {n curr}))))
      )))
