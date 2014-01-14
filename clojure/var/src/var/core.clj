(ns var.core)

(def ^:dynamic x)

(defn incx  [] (set! x (inc x)))
(defn decx  [] (set! x (dec x)))

(println
 (binding [x 0]
   (reset)
   (incx)
   (incx)
   (decx)))
