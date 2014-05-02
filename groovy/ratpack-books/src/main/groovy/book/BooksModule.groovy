package book

import com.google.inject.AbstractModule
import com.google.inject.Scopes

@groovy.transform.CompileStatic
class BooksModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(JsonRenderer)
        bind(Repository).in(Scopes.SINGLETON)
    }

}
