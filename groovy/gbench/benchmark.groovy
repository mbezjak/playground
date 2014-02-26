@Grab("org.gperfutils:gbench:0.4.2-groovy-2.1")

def r = benchmark(warmUpTime:30) {
    'StringBuilder' {
        def sb = new StringBuilder()
        sb.append('foo')
        sb.append('bar')
        sb.append('baz')
        sb.toString()
    }
    'StringBuffer' {
        def sb = new StringBuffer()
        sb.append('foo')
        sb.append('bar')
        sb.append('baz')
        sb.toString()
    }
}

r.prettyPrint()
