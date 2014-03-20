package app

class Use {

    static dsl = {
        property 'foo'
        property 'foo.bar'

        a
        b
        c.d.e
    }

}
