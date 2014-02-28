@Grab("org.gperfutils:gprof:0.3.0-groovy-2.1")

def fib(n) {
    if (n < 2) n
    else       fib(n-1) + fib(n-2)
}

profile(excludeMethods: ['java.*']) {
    fib(20)
}.prettyPrint()
