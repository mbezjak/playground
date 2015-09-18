(use '[clojure.test :only (is)])

(defn cut-node [target [root & children]]
  (if (= target root)
    nil
    (apply list root (remove nil? (map #(cut-node target %) children)))))

(is (= '(a (b (c (g) (h))))
       (cut-node 'd '(a (b (c (g) (h)) (d (e) (f)))))))

(defn find-target
  ([target tree] (find-target target tree nil))
  ([target [root & children] parent]
   (if (= target root)
     {:children children, :parent parent}
     (some #(find-target target % root) children))))

(is (= (find-target 'd '(a (b (c (g) (h)) (d (e) (f)))))
       {:children '((e) (f)), :parent 'b}))

(defn reparent [new-root tree]
  (let [{:keys [children parent]} (find-target new-root tree)
        parent-as-right-tree (if parent [(reparent parent (cut-node new-root tree))] [])]
    (apply list new-root (concat children parent-as-right-tree))))

(defn reparent-4clojure [new-root tree]
  (letfn [(cut-node [target [root & children]]
            (if (= target root)
              nil
              (apply list root (remove nil? (map #(cut-node target %) children)))))
          (find-target [target tree] (find-target-with-parent target tree nil))
          (find-target-with-parent [target [root & children] parent]
            (if (= target root)
              {:children children, :parent parent}
              (some #(find-target-with-parent target % root) children)))]

    (let [{:keys [children parent]} (find-target new-root tree)
          parent-as-right-tree (if parent [(reparent-4clojure parent (cut-node new-root tree))] [])]
      (apply list new-root (concat children parent-as-right-tree)))))


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
