@Grab('com.aestasit.infrastructure.sshoogr:sshoogr:0.9.18')
import com.aestasit.infrastructure.ssh.dsl.SshDslEngine
import com.aestasit.infrastructure.ssh.SshOptions

if (args.length != 1) {
    println "Usage: groovy script.groovy SSH_URL"
    System.exit(1)
}


def url = args[0]

def options = new SshOptions()
options.trustUnknownHosts = true
def engine = new SshDslEngine(options)

engine.remoteSession(url) {
    exec 'ls -al /tmp'
    remoteFile('/tmp/sshoogr.txt').text = 'example usage'
    exec 'ls -al /tmp | grep sshoogr'

    exec command: 'command_does_not_exist', failOnError: false
}
