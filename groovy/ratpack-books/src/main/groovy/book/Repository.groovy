package book

class Repository {
    private final Map<String, Map> books = [:]

    @groovy.transform.WithWriteLock
    void put(String name, Map book) {
        books[name] = clone(book)
    }

    @groovy.transform.WithReadLock
    Map get(String name) {
        clone(books[name])
    }

    @groovy.transform.WithReadLock
    Set<String> names() {
        books.keySet()
    }

    private Map clone(Map book) {
        new HashMap(book ?: [:]).asImmutable()
    }
}
