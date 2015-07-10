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

(defn lines [str] (clojure.string/split str #"\n"))

(def shifts {:north [-1 0] :west [0 -1] :east [1 0] :south [0 1]})
(defn neighbors [coords]
  "Turn x,y coord into set of shifted neighbor coords
   e.g [0 0] -> #{[1 0] [0 1] [-1 0] [0 -1]}"
  (into #{} (map vec
                 (for [shift (vals shifts)]
                   (map #(apply + %) (map vector coords shift))))))

(defn graphed [coord-map]
  (reduce (fn [cm [coords value]]
            (assoc cm coords (into {:value value} {:neighbors (neighbors coords)})))
          {}
          coord-map))

(defn parse-landscape [landscape-string]
  (graphed (coord-map (coords (lines landscape-string)))))

(defn landscape-file [filename]
  (slurp (.getFile (clojure.java.io/resource filename))))

(defn start-pos [ls-map]
  (first (first (filter (fn [pair] (= "S" (last pair))) ls-map))))
