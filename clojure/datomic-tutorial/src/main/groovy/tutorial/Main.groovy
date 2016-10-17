package tutorial

import datomic.Peer

class Main {

    static void main(String[] args) {
        def uri = 'datomic:mem://seattle'
        Peer.createDatabase(uri)
    }

}
