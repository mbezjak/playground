package app

class Runner {

    static void run(Closure config, Object delegate) {
        def dsl = config.clone()

        dsl.delegate = delegate
        dsl.resolveStrategy = Closure.DELEGATE_ONLY
        dsl.run()
    }

}
