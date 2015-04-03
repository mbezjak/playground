(use '[clojure.test :only (is)])

(defn balanced? [string]
  (letfn [(is-open-paren [c]
            (#{\( \[ \{} c))
          (is-close-paren [c]
            (#{\) \] \}} c))
          (open-of [c]
            (condp = c
              \) \(
              \] \[
              \} \{))
          (machine [open-parens chars]
            (if (empty? chars)
              (empty? open-parens)

              (let [[c & rst] chars]
                (cond
                  (is-open-paren c) (partial machine (cons c open-parens) rst)
                  (and (is-close-paren c) (empty? open-parens)) false
                  (and (is-close-paren c) (= (open-of c) (first open-parens))) (partial machine (rest open-parens) rst)
                  :else (partial machine open-parens rst)))))]

    (trampoline machine '() string)))

(is (balanced? "This string has no brackets."))

(is (balanced? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))

(is (not (balanced? "(start, end]")))

(is (not (balanced? "())")))

(is (not (balanced? "[ { ] } ")))

(is (balanced? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))

(is (not (balanced? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))

(is (not (balanced? "[")))
