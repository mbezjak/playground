(use '[clojure.test :only (is)])

(defn cut-children [node [root & children]]
  (if (= node root)
    (list root)
    (apply list root (map #(cut-children node %) children))))

(is (= '(a (b (c (g) (h)) (d)))
       (cut-children 'd '(a (b (c (g) (h)) (d (e) (f)))))))

ee
(defn reparent [root tree])


(is (= '(n)
       (reparent 'n '(n))))

(is (= '(a (t (e)))
       (reparent 'a '(t (e) (a)))))

(is (= '(e (t (a)))
       (reparent 'e '(a (t (e))))))

(is (= '(a (b (c)))
       (reparent 'a '(c (b (a))))))

(is (= '(d
         (b
          (c)
          (e)
          (a
           (f
            (g)
            (h)))))
       (reparent 'd '(a
                      (b
                       (c)
                       (d)
                       (e))
                      (f
                       (g)
                       (h))))))

(is (= '(c
         (d)
         (e)
         (b
          (f
           (g)
           (h))
          (a
           (i
            (j
             (k)
             (l))
            (m
             (n)
             (o))))))
       (reparent 'c '(a
                      (b
                       (c
                        (d)
                        (e))
                       (f
                        (g)
                        (h)))
                      (i
                       (j
                        (k)
                        (l))
                       (m
                        (n)
                        (o)))))))
