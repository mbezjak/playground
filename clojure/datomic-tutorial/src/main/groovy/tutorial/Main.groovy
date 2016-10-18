package tutorial

import datomic.Peer
import datomic.Connection
import datomic.Util

class Main {

    static void main(String[] args) {
        def uri = 'datomic:mem://seattle'
        Peer.createDatabase(uri)
        def conn = Peer.connect(uri)

        def schemaReader = new FileReader("src/main/resources/seattle-schema.edn")
        def schemaTx = Util.readAll(schemaReader).get(0)
        schemaReader.close()
        def schemaTxResult = conn.transact(schemaTx).get()

        def dataReader = new FileReader("src/main/resources/seattle-data0.edn")
        def dataTx = Util.readAll(dataReader).get(0)
        dataReader.close()
        def dataTxResult = conn.transact(dataTx).get()

        def db = conn.db()

        def query = "[:find ?c :where [?c :community/name]]"
        def results = Peer.query(query, db)
        def firstEntity = db.entity(results.first().first())
        println "$query size = ${results.size()}"
        println "$query first entity = ${firstEntity}; name = ${firstEntity.get(':community/name')}"
    }

}
