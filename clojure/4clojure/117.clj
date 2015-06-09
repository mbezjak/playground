(use '[clojure.test :only (is)])
(use '[clojure.set])

(defn movements-from-point [[x y]]
  #{[(dec x) y]
    [(inc x) y]
    [x (dec y)]
    [x (inc y)]})

(is (= (movements-from-point [2 2])
       #{[1 2]
         [3 2]
         [2 1]
         [2 3]}))

(defn out-of-bounds? [maze [x y]]
  (let [rows (count maze)
        cols (if (zero? rows) 0 (count (first maze)))]
    (or
     (< x 0)
     (< y 0)
     (>= x rows)
     (>= y cols))))

(def sample-maze
  ["#####"
   "#M C#"
   "#####"])

(is (= false (out-of-bounds? sample-maze [1 1])))
(is (= true  (out-of-bounds? sample-maze [-1 1])))
(is (= true  (out-of-bounds? sample-maze [4 1])))
(is (= true  (out-of-bounds? sample-maze [1 9])))

(defn free-to-walk [maze point]
  (not= \# (get-in maze point)))

(is (= false (free-to-walk sample-maze [0 0])))
(is (= true  (free-to-walk sample-maze [1 2])))
(is (= true  (free-to-walk sample-maze [1 3])))

(defn next-frontier [maze current-frontier visited]
  (let [potential-movements (mapcat movements-from-point current-frontier)
        walkable-movements (filter #(and (not (out-of-bounds? maze %))
                                         (free-to-walk maze %))
                                   potential-movements)]
    (clojure.set/difference (set walkable-movements) visited)))

(is (= (next-frontier sample-maze #{[1 1]} #{}) #{[1 2]}))
(is (= (next-frontier sample-maze #{[1 1]} #{[1 2]}) #{}))

(defn starting-position [maze]
  (first
   (for [[ridx row] (map-indexed vector maze)
         [cidx tile] (map-indexed vector row)
         :when (= tile \M)]
     [ridx cidx])))

(is (= [1 1] (starting-position sample-maze)))

(defn solvable? [maze])
ee
(is (= true  (solvable? ["M   C"])))

(is (= false (solvable? ["M # C"])))

(is (= true  (solvable? ["#######"
                         "#     #"
                         "#  #  #"
                         "#M # C#"
                         "#######"])))

(is (= false (solvable? ["########"
                         "#M  #  #"
                         "#   #  #"
                         "# # #  #"
                         "#   #  #"
                         "#  #   #"
                         "#  # # #"
                         "#  #   #"
                         "#  #  C#"
                         "########"])))

(is (= false (solvable? ["M     "
                         "      "
                         "      "
                         "      "
                         "    ##"
                         "    #C"])))

(is (= true  (solvable? ["C######"
                         " #     "
                         " #   # "
                         " #   #M"
                         "     # "])))

(is (= true  (solvable? ["C# # # #"
                         "        "
                         "# # # # "
                         "        "
                         " # # # #"
                         "        "
                         "# # # #M"])))
