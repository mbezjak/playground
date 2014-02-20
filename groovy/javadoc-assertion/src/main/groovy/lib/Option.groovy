package lib

@groovy.transform.Canonical
class Option<T> {
    final T val

    boolean isEmpty() { val == null }

    /**
     * <pre class="groovyTestCase">
     * import lib.Option
     * def a = new Option(null)
     * def b = new Option(42)
     * def f = { x -> x + 100 }
     *
     * assert a.collect(f) == a
     * assert b.collect(f) == new Option(142)
     * </pre>
     */
    def <V> Option<V> collect(Closure<V> f) {
        empty ? this : new Option(f(val))
    }
}
