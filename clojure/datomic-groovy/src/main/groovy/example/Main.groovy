package example

import datomic.Peer

class Main {

    static void main(String[] args) {
        def uri = "datomic:mem://hello"
        Peer.createDatabase(uri)

        def conn = Peer.connect(uri)
        def datom = ["db/add", Peer.tempid("db.part/user"),
                     "db/doc", "hello world"]
        def resp = conn.transact([datom]).get()
        println "Transact response = $resp"

        def db = conn.db()
        def helloEntity = Peer.query('[:find ?entity :where [?entity :db/doc "hello word"]]', db)
        println "Hello entity = $helloEntity"

        def count = Peer.query('[:find (count ?entity) . :where [?entity :db/doc]]', db)
        println "Count = $count"

        def docEntities = Peer.query('[:find ?entity :where [?entity :db/doc]]', db)
        println "Doc entities = $docEntities"
    }

}
