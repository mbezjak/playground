(use '[clojure.test :only (is)])
(require 'clojure.set)

(defn whitespace? [c]
  (= c \ ))

(defn hash? [c]
  (= c \#))

(defn underscore? [c]
  (= c \_))

(defn strip-whitespaces [board]
  (vec
   (for [row board]
     (apply str (filter #(not (whitespace? %)) row)))))

(is (= ["_#__e"] (strip-whitespaces ["_ # _ _ e"])))
(is (= ["c___"
        "d_#e"
        "ry__"]
       (strip-whitespaces ["c _ _ _"
                           "d _ # e"
                           "r y _ _"])))

(defn to-matrix [board]
  (vec (map vec board)))

(is (= [[\_ \# \_ \_ \e]] (to-matrix ["_#__e"])))

(defn legal-placements-in-row [row]
  (->> row
       (partition-by hash?)
       (filter #(not= % [\#]))
       (set)))

(is (= #{[\_] [\_ \_ \e]}
       (legal-placements-in-row [\_ \# \_ \_ \e])))

(is (= #{[\_] [\_ \_] [\e]}
       (legal-placements-in-row [\_ \# \_ \_ \# \e])))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn legal-placements [matrix]
  (clojure.set/union (apply clojure.set/union (map legal-placements-in-row matrix))
                     (apply clojure.set/union (map legal-placements-in-row (transpose matrix)))))

(is (= #{[\c \_ \_ \_]
         [\d \_]
         [\e]
         [\r \y \_ \_]
         [\c \d \r]
         [\_ \_ \y]
         [\_]
         [\_ \e \_]}
       (legal-placements [[\c \_ \_ \_]
                          [\d \_ \# \e]
                          [\r \y \_ \_]])))

(defn can-match-row? [string row]
  (if (= (count row) (count string))
    (every? (fn [[x y]]
              (cond
                (= x y) true
                (underscore? x) true
                :else false))
            (map vector row string))
    false))

(is (= false (can-match-row? "abc" [\a])))
(is (= true (can-match-row? "abc" [\a \b \c])))
(is (= true (can-match-row? "abc" [\a \_ \_])))

(defn solve [string placements]
  (->> placements
       (filter #(can-match-row? string %))
       (empty?)
       (not)))

(is (= false
       (solve "the" #{[\c \_ \_ \_]
                      [\d \_]
                      [\e]
                      [\r \y \_ \_]
                      [\c \d \r]
                      [\_ \_ \y]
                      [\_]
                      [\_ \e \_]})))

(defn can-place? [string board]
  (->> board
       (strip-whitespaces)
       (to-matrix)
       (legal-placements)
       (solve string)))

(defn can-place?-4clojure [string board]
  (letfn [(whitespace? [c] (= c \ ))
          (hash? [c] (= c \#))
          (underscore? [c] (= c \_))
          (strip-whitespaces [board]
            (vec
             (for [row board]
               (apply str (filter #(not (whitespace? %)) row)))))
          (to-matrix [board] (vec (map vec board)))
          (legal-placements-in-row [row]
            (->> row
                 (partition-by hash?)
                 (filter #(not= % [\#]))
                 (set)))
          (transpose [matrix] (apply mapv vector matrix))
          (legal-placements [matrix]
            (clojure.set/union (apply clojure.set/union (map legal-placements-in-row matrix))
                               (apply clojure.set/union (map legal-placements-in-row (transpose matrix)))))
          (can-match-row? [string row]
            (if (= (count row) (count string))
              (every? (fn [[x y]]
                        (cond
                          (= x y) true
                          (underscore? x) true
                          :else false))
                      (map vector row string))
              false))
          (solve [string placements]
            (->> placements
                 (filter #(can-match-row? string %))
                 (empty?)
                 (not)))]

    (->> board
         (strip-whitespaces)
         (to-matrix)
         (legal-placements)
         (solve string))))

(is (= true  (can-place? "the" ["_ # _ _ e"])))

(is (= false (can-place? "the" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))

(is (= true  (can-place? "joy" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))

(is (= false (can-place? "joy" ["c o n j"
                                "_ _ y _"
                                "r _ _ #"])))

(is (= true  (can-place? "clojure" ["_ _ _ # j o y"
                                    "_ _ o _ _ _ _"
                                    "_ _ f _ # _ _"])))
