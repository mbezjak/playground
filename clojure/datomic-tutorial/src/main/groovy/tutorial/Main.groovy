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
        forAttributeValue(db)
        onlyAttributeValue(db)
        multiAttributeValue(db)
        cardinalityMany(db)
        byAttributeValue(db)
        acrossReferences(db)
        acrossReferencesWithRegion(db)
        parametrized(db)
        parametrizedList(db)
        parametrizedTuples(db)
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

    private static void forAttributeValue(db) {
        def query = "[:find ?c ?n :where [?c :community/name ?n]]"
        def results = Peer.query(query, db)
        def names = results*.get(1).take(10)
        println "Community names = $names"
    }

    private static void onlyAttributeValue(db) {
        def query = "[:find [?n ...] :where [_ :community/name ?n]]"
        def results = Peer.query(query, db)
        def names = results.take(10)
        println "Only community names = $names"
    }

    private static void multiAttributeValue(db) {
        def query = "[:find ?n ?u :where [?c :community/name ?n] [?c :community/url ?u]]"
        def results = Peer.query(query, db)
        println "Community name + url = ${results.take(10)}"
    }

    private static void cardinalityMany(db) {
        def query = '[:find ?e ?c :where [?e :community/name "belltown"] [?e :community/category ?c]]'
        def results = Peer.query(query, db)
        println "Cardinality many = ${results.take(10)}"
    }

    private static void byAttributeValue(db) {
        def query = "[:find [?n ...] :where [?c :community/name ?n] [?c :community/type :community.type/twitter]]"
        def results = Peer.query(query, db)
        println "Community names = ${results}"
    }

    private static void acrossReferences(db) {
        def query = """
            [:find [?c_name ...]
             :where
             [?c :community/name ?c_name]
             [?c :community/neighborhood ?n]
             [?n :neighborhood/district ?d]
             [?d :district/region :region/ne]]
        """
        def results = Peer.query(query, db)
        println "Community names in NE region = ${results}"
    }

    private static void acrossReferencesWithRegion(db) {
        def query = """
            [:find ?c_name ?r_name
             :where
             [?c :community/name ?c_name]
             [?c :community/neighborhood ?n]
             [?n :neighborhood/district ?d]
             [?d :district/region ?r]
             [?r :db/ident ?r_name]]
        """
        def results = Peer.query(query, db)
        println "Community names and region = ${results.take(10)}"
    }

    private static void parametrized(db) {
        def query = """
            [:find [?n ...]
            :in \$ ?t
            :where
            [?c :community/name ?n]
            [?c :community/type ?t]]
        """
        def results = Peer.query(query, db, ":community.type/facebook-page")
        println "Parametrized community names = ${results}"
    }

    private static void parametrizedList(db) {
        def query = """
            [:find ?n ?t
            :in \$ [?t ...]
            :where
            [?c :community/name ?n]
            [?c :community/type ?t]]
        """
        def results = Peer.query(query, db, [":community.type/facebook-page", ":community.type/twitter"])
        println "Parametrized list community name and type = ${results}"
    }

    private static void parametrizedTuples(db) {
        def query = """
            [:find ?n ?t ?ot
            :in \$ [[?t ?ot]]
            :where
            [?c :community/name ?n]
            [?c :community/type ?t]
            [?c :community/orgtype ?ot]]
        """
        def results = Peer.query(query, db, [
            [":community.type/email-list", ":community.orgtype/community"],
            [":community.type/website", ":community.orgtype/commercial"]
        ])
        println "Parametrized tuples communities = ${results}"
    }

}
