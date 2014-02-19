package app

class Main {

    static final name    = 'jenkins'
    static final version = '1.0'

    static void main(String[] args) {
        def cli     = define()
        def options = cli.parse(args)
        def jobs    = options?.arguments()

        if (!options) cli.usage()
        else if (options.h) cli.usage()
        else if (options.v) println "$name $version"
        else if (jobs.isEmpty()) cli.usage()
        else if (!jobs.isEmpty()) run(jobs, options.s ?: 'build')
        else cli.usage()
    }

    static CliBuilder define() {
        def cli = new CliBuilder(
            usage  : "$name [options] jobName...",
            header : "Start the build for all the given jobs\n",
            footer : "\nTry: $name --server=build-doc a b c"
        )

        cli.h(longOpt: 'help', 'Show usage')
        cli.v(longOpt: 'version', 'Show program version')
        cli.s(longOpt: 'server', args:1, argName: 'server-name', 'Operate on a given server instance')

        cli
    }

    static void run(List<String> jobs, String server) {
        def cmds = jobs.collect { job ->
            "curl http://$server/$job/build"
        }
        def all = cmds.join('\n')

        println "Something like:\n$all"
    }

}
