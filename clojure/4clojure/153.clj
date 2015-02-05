(require '[clojure.set :as set])

(defn f [xs]
  (let [num-elems-separate (apply + (map count xs))
        num-elems-combined (count (apply set/union xs))]
    (= num-elems-combined num-elems-separate)))

(assert (= (f #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
           true))

(assert (= (f #{#{:a :b :c :d :e}
                 #{:a :b :c :d}
                 #{:a :b :c}
                 #{:a :b}
                 #{:a}})
           false))

(assert (= (f #{#{[1 2 3] [4 5]}
                 #{[1 2] [3 4 5]}
                 #{[1] [2] 3 4 5}
                 #{1 2 [3 4] [5]}})
           true))

(assert (= (f #{#{'a 'b}
                 #{'c 'd 'e}
                 #{'f 'g 'h 'i}
                 #{''a ''c ''f}})
           true))

(assert (= (f #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                 #{#{:x :y :z} #{:x :y} #{:z} #{}}
                 #{'[:x :y :z] [:x :y] [:z] [] {}}})
           false))

(assert (= (f #{#{(= "true") false}
                 #{:yes :no}
                 #{(class 1) 0}
                 #{(symbol "true") 'false}
                 #{(keyword "yes") ::no}
                 #{(class '1) (int \0)}})
           false))

(assert (= (f #{#{distinct?}
                 #{#(-> %) #(-> %)}
                 #{#(-> %) #(-> %) #(-> %)}
                 #{#(-> %) #(-> %) #(-> %)}})
           true))

(assert (= (f #{#{(#(-> *)) + (quote mapcat) #_ nil}
                 #{'+ '* mapcat (comment mapcat)}
                 #{(do) set contains? nil?}
                 #{, , , #_, , empty?}})
           false))
