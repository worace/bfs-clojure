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

(defn parse-landscape [landscape-string]
  (coord-map (coords (lines landscape-string))))

(defn landscape-file [filename]
  (slurp (.getFile (clojure.java.io/resource filename))))

(defn start-pos [ls-map]
  (first (first (filter (fn [pair] (= "S" (last pair))) ls-map))))

(defn shift-coord [shift coord]
  [(+ (get shift 0) (get coord 0)) (+ (get shift 1) (get coord 1))])

(defn neighbors [coords]
  (let [shifts [[0 -1] [-1 0] [1 0] [0 1]]]
    (map (fn [pair] (apply shift-coord pair)) (map vector shifts (repeat coords)))))
