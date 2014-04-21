(defprotocol SHOW
  (show [val]))

(extend-type Integer
  SHOW
  (show [i] (.toString i)))

(extend-type Long
  SHOW
  (show [i] (.toString i)))

(extend-type clojure.lang.IPersistentVector
  SHOW
  (show [i] (.toString i)))


(println (show 42))
(println (show (int 53)))
(println (show [1 2 3 4]))
; (show "foo") => no protocol impl


(defrecord Name [last first])

(defn name-desc [name]
  (str (:last name) " " (:first name)))

(println (name-desc (Name. "John" "Doe")))

(extend-type Name
  SHOW
  (show [n]
    (name-desc n)))

(println (show (Name. "Jane" "Doe")))
