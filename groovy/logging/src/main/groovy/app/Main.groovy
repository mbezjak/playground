package app

class Main {

    static void main(String[] args) {
        new Configurer().configure()
    }

    @groovy.util.logging.Slf4j
    static class Configurer {
        def configure() {
            log.info 'Starting to configure the application'
        }
    }

}
