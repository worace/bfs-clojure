(ns bfs-clojure.landscape)


(def shifts {:north [-1 0] :west [0 -1] :east [1 0] :south [0 1]})
(defn adj-coords [coords]
  "Turn x,y coord into set of shifted neighbor coords
   e.g [0 0] -> #{[1 0] [0 1] [-1 0] [0 -1]}"
  (into #{} (map vec
                 (for [shift (vals shifts)]
                   (map #(apply + %) (map vector coords shift))))))

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

(defn coord-map [coords]
  "take seq of coordinate-value tuples and return
   map of [x y] -> 'value' coords."
  (apply hash-map (mapcat identity coords)))

(defn coords [rows]
  "take seq of strings representing map rows
   and parse them into sequence of coordinate, position-value
   tuples. e.g (([0 0] 'a') ([1 0] 'b'))
   Use mapcat identity as a 1-level flatten"
  (mapcat identity
          (map-indexed (fn [y row]
                         (map-indexed
                          (fn [x char] [[x y] (str char)])
                          row))
                       rows)))

(defn coord-map [coords]
  "take seq of coordinate-value tuples and return
   map of [x y] -> 'value' coords."
  (apply hash-map (mapcat identity coords)))

(defn parse-landscape [landscape-string]
  "Parse newline-delimited string representing a landscape
   into map of [x y] coord to maps of positions containing
   :value and :adj(acent coords)"
  (graphed (coord-map (coords (lines landscape-string)))))

(def from-file (comp parse-landscape landscape-file))
