(ns exercise.core)

(defn read-first []
  (print "What is the first number? ")
  (flush)
  (read-string (read-line)))

(defn read-second []
  (print "What is the second number? ")
  (flush)
  (read-string (read-line)))

(defn output [x y]
  (str
   x " + " y " = " (+ x y) "\n"
   x " - " y " = " (- x y) "\n"
   x " * " y " = " (* x y) "\n"
   x " / " y " = " (double (/ x y)) "\n"))

(defn -main [& args]
  (let [x (read-first)
        y (read-second)]
    (print (output x y))
    (flush)))
