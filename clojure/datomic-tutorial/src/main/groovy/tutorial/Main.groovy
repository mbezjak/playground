package tutorial

import datomic.Peer
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

        simpleFind(db)
        simplePull(db)
        neighborhood(db)
        reverseDirection(db)
    }

    private static void simpleFind(db) {
        def query = "[:find ?c :where [?c :community/name]]"
        def results = Peer.query(query, db)
        def firstEntity = db.entity(results.first().first())
        def name = firstEntity.get(':community/name')
        def keys = firstEntity.keySet()
        println "$query size = ${results.size()}"
        println "$query first entity = ${firstEntity}; name = $name; keys = $keys"
    }

    private static void simplePull(db) {
        def query = "[:find (pull ?c [*]) :where [?c :community/name]]"
        def results = Peer.query(query, db)
        println "$query first result = ${results.first()}"
    }

    private static void neighborhood(db) {
        def query = "[:find ?c :where [?c :community/name]]"
        def results = Peer.query(query, db)
        def entity = db.entity(results.first().first())
        def neighborhood = entity.get(':community/neighborhood')
        def name = neighborhood.get(':neighborhood/name')
        println "neighborhood name = $name"
    }

    private static void reverseDirection(db) {
        def query = "[:find ?c :where [?c :community/name]]"
        def results = Peer.query(query, db)
        def community = db.entity(results.first().first())
        def neighborhood = community.get(':community/neighborhood')
        def communities = neighborhood.get(':community/_neighborhood')*.get(':community/name')
        println "Communities for neighborhood=${neighborhood}: $communities"
    }

}
