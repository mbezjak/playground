(ns core)

;; https://github.com/borkdude/sci
(require '[sci.core :as sci])

(sci/eval-string "(inc 1)")
(sci/eval-string "(inc x)")
(sci/eval-string "(inc x)" {:bindings {'x 2}})

(sci/init {:bindings {'x 2}})
(sci/init {:bindings {'x 2}
           :allow ['inc]})
(sci/eval-form (sci/init {:bindings {'x 2}}) '(inc x))
(sci/eval-form (sci/init {:bindings {'x 2}
                          :allow ['inc]})
               '(inc x))
(sci/eval-form (sci/init {:bindings {'x 2}
                          :allow ['inc 'x]})
               '(inc x))


(sci/eval-form (sci/init {:bindings {'w 80 'h 1.8}
                          :allow ['+ '- '/ '* 'w 'h]})
               '(/ w (* h h)))


(java.util.UUID/randomUUID)
(sci/eval-form (sci/init {:allow []})
               '(java.util.UUID/randomUUID))
(sci/eval-string "(java.util.UUID/randomUUID)" {:classes {'java.util.UUID java.util.UUID}})
