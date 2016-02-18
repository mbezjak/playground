(defn monte-carlo [trials experiment]
  (let [results (->> (range trials)
                     (map (fn [_] (experiment)))
                     (frequencies))]
    (/ (get results true 0) trials)))

(defn in-rectangle [[x y]]
  (and (> x 0)
       (< x 9)
       (> y 0)
       (< y 9)))

(defn random-in-range [limit]
  (int (* limit (Math/random))))

(defn rectangle-area-estimate []
  (in-rectangle [(random-in-range 100) (random-in-range 100)]))

(defn ratio-as-percentage [ratio]
  (* 100 (double ratio)))

(printf "Rectangle area as percentage of total area\nReal      = %.2f%%\nEstimated = %.2f%%\n"
        (ratio-as-percentage (/ (* 8 8) (* 100 100)))
        (ratio-as-percentage (monte-carlo 10000 rectangle-area-estimate)))
