package tutorial

import datomic.Peer
import datomic.Util
import datomic.Connection

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
        function(db)
        fulltext(db)
        fulltextWithParameter(db)
        rulesSimple(db)
        rulesComplex(db)

        def (dataTxDate, schemaTxDate) = txInstant(db)
        assert count(db.asOf(schemaTxDate))  == 0
        assert count(db.asOf(dataTxDate))    == 150
        assert count(db.since(schemaTxDate)) == 150
        assert count(db.since(dataTxDate))   == 0
        assert count(db.asOf(new Date()))    == 150

        def newDataReader = new FileReader("src/main/resources/seattle-data1.edn")
        def newDataTx = Util.readAll(newDataReader).get(0)
        newDataReader.close()
        def dbIfNewData = db.with(newDataTx).get(Connection.DB_AFTER)

        assert count(dbIfNewData) == 258
        assert count(db)          == 150
        assert count(conn.db())   == 150

        def newDataTxResult = conn.transact(newDataTx).get()
        assert count(conn.db().since(dataTxDate)) == 108

        insertNew(conn)
        assert count(conn.db()) == 259
        retractUrl(conn)
        update(conn)
        replaceCategory(conn)
        replaceEntity(conn)
        retractEntity(conn)

        Peer.shutdown(true)
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

    private static void function(db) {
        def query = """
            [:find [?n ...]
            :where
            [?c :community/name ?n]
            [(.compareTo ?n "C") ?res]
            [(< ?res 0)]]
        """
        def results = Peer.query(query, db)
        println "Function in query = $results"
    }

    private static void fulltext(db) {
        def query = """
            [:find [?n ...]
            :where
            [(fulltext \$ :community/name "wallingford") [[?e ?n]]]]
        """
        def results = Peer.query(query, db)
        println "Fulltext = $results"
    }

    private static void fulltextWithParameter(db) {
        def query = """
            [:find ?name ?cat
            :in \$ ?type ?search
            :where
            [?c :community/name ?name]
            [?c :community/type ?type]
            [(fulltext \$ :community/category ?search) [[?c ?cat]]]]
        """
        def results = Peer.query(query, db, ":community.type/website", "food")
        println "Fulltext with parameters = $results"
    }

    private static void rulesSimple(db) {
        def rules = """
            [[[region ?c ?r]
              [?c :community/neighborhood ?n]
              [?n :neighborhood/district ?d]
              [?d :district/region ?re]
              [?re :db/ident ?r]]]
        """
        def query = """
            [:find [?n ...]
            :in \$ %
            :where
            [?c :community/name ?n]
            (region ?c :region/ne)]
        """
        def results = Peer.query(query, db, rules)
        println "Rules simple = $results"
    }

    private static void rulesComplex(db) {
        def rules = """
            [[[region ?c ?r]
              [?c :community/neighborhood ?n]
              [?n :neighborhood/district ?d]
              [?d :district/region ?re]
              [?re :db/ident ?r]]
             [[social-media ?c]
              [?c :community/type :community.type/twitter]]
             [[social-media ?c]
              [?c :community/type :community.type/facebook-page]]
             [[northern ?c] (region ?c :region/ne)]
             [[northern ?c] (region ?c :region/n)]
             [[northern ?c] (region ?c :region/nw)]
             [[southern ?c] (region ?c :region/sw)]
             [[southern ?c] (region ?c :region/s)]
             [[southern ?c] (region ?c :region/se)]]
        """
        def query = """
            [:find [?n ...]
            :in \$ %
            :where
            [?c :community/name ?n]
            (southern ?c)
            (social-media ?c)]
        """
        def results = Peer.query(query, db, rules)
        println "Rules complex = $results"
    }

    private static List<Date> txInstant(db) {
        def query = """
            [:find [?when ...]
            :where
            [?tx :db/txInstant ?when]]
        """
        def results = Peer.query(query, db)
        println "Tx instant = $results"

        results.sort(false).reverse().take(2)
    }

    private static int count(db) {
        def query = """
            [:find [?c ...]
            :where
            [?c :community/name]]
        """
        def results = Peer.query(query, db)
        results.size()
    }

    private static void insertNew(conn) {
        def tx = """
            [{:db/id #db/id[:db.part/user]
              :community/name "Foo"
              :community/url "http://example.com"
              :community/neighborhood [:neighborhood/name "Alki"]
              :community/category ["Example users"]
              :community/orgtype :community.orgtype/personal
              :community/type :community.type/wiki
            }]
        """
        conn.transact(Util.readAll(new StringReader(tx)).get(0)).get()
    }

    private static String fooCommunityUrl(db) {
        def query = """
            [:find ?u .
             :where
             [?e :community/name "Foo"]
             [?e :community/url ?u]]
        """

        Peer.query(query, db)
    }

    private static void retractUrl(conn) {
        assert fooCommunityUrl(conn.db()) == 'http://example.com'
        def communityId = Peer.query('[:find ?e . :where [?e :community/name "Foo"]]', conn.db())

        def doesNothingBecauseUrlDoesNotMatch = "[[:db/retract $communityId :community/url \"ewq\"]]"
        conn.transact(Util.read(doesNothingBecauseUrlDoesNotMatch)).get()
        assert fooCommunityUrl(conn.db()) == 'http://example.com'

        def retract = "[[:db/retract $communityId :community/url \"http://example.com\"]]"
        conn.transact(Util.read(retract)).get()
        assert fooCommunityUrl(conn.db()) == null
    }

    private static void update(conn) {
        assert fooCommunityUrl(conn.db()) == null
        def communityId = Peer.query('[:find ?e . :where [?e :community/name "Foo"]]', conn.db())

        def update = "[{:db/id $communityId :community/url \"http://another.example.com\"}]"
        conn.transact(Util.read(update)).get()
        assert fooCommunityUrl(conn.db()) == 'http://another.example.com'

        def updateAgain = "[[:db/add $communityId :community/url \"http://again.example.com\"]]"
        conn.transact(Util.read(updateAgain)).get()
        assert fooCommunityUrl(conn.db()) == 'http://again.example.com'

        def addCategory = "[{:db/id $communityId :community/category \"New\"}]"
        conn.transact(Util.read(addCategory)).get()
        def queryForCategory = '[:find [?c ...] :where [?e :community/name "Foo"] [?e :community/category ?c]]'
        assert Peer.query(queryForCategory, conn.db()) as Set == ['Example users', 'New'] as Set
    }

    private static int countFooCommunity(db) {
        def query = """
            [:find ?e
             :where
             [?e :community/name "Foo"]]
        """

        Peer.query(query, db).size()
    }

    private static void replaceCategory(conn) {
        // http://stackoverflow.com/questions/36356933/updating-transaction-in-datomic-for-an-attribute-that-has-many-cardinality
        def fn = """
            [{:db/ident :bsu.fns/replace-to-many-scalars,
              :db/doc "Given an entity's lookup ref, a to-many (scalar) attribute, and a list of new values,
             yields a transaction that replaces the old values by new ones"
              :db/id #db/id[:db.part/user],
              :db/fn #db/fn {
                   :lang :clojure,
                   :imports [],
                   :requires [[datomic.api :as d]],
                   :params [db entid attr new-vals],
                   :code (let [old-vals (if-let [e (d/entity db entid)] (get e attr) ())
                               to-remove (remove (set (seq new-vals)) old-vals)]
                           (concat
                             (for [ov to-remove] [:db/retract entid attr ov])
                             (for [nv new-vals] [:db/add entid attr nv]))
                           )}}]
        """
        conn.transact(Util.readAll(new StringReader(fn)).get(0)).get()

        def communityId = Peer.query('[:find ?e . :where [?e :community/name "Foo"]]', conn.db())
        def replace = "[[:bsu.fns/replace-to-many-scalars $communityId :community/category [\"New\"]]]"
        conn.transact(Util.read(replace)).get()
        def queryForCategory = '[:find [?c ...] :where [?e :community/name "Foo"] [?e :community/category ?c]]'
        assert Peer.query(queryForCategory, conn.db()) as Set == ['New'] as Set
    }

    private static void replaceEntity(conn) {
        def fn = """
            [{:db/ident :fns/replace-entity,
              :db/id #db/id[:db.part/user],
              :db/fn #db/fn {
                   :lang :clojure,
                   :imports [],
                   :requires [[datomic.api :as d]],
                   :params [db entid attrs],
                   :code (let [to-remove (if-let [e (d/entity db entid)]
                                (->> e
                                     (filter (comp not #{:db/id} key))
                                     (filter (fn [[key value]]
                                               (not= (if (instance? datomic.Entity value) (:db/id value) value) (get attrs key)))))
                                [])]
                           (concat
                             (for [[key value] to-remove]
                                 (cond
                                   (instance? datomic.Entity value) [:db/retract entid key (:db/id value)]
                                   (or (sequential? value) (set? value)) [:bsu.fns/replace-to-many-scalars entid key []]
                                   :else [:db/retract entid key value]))
                             [(merge {:db/id entid} attrs)])
                           )}}]
        """
        conn.transact(Util.readAll(new StringReader(fn)).get(0)).get()

        def communityId = Peer.query('[:find ?e . :where [?e :community/name "Foo"]]', conn.db())
        def neighborhoodId = Peer.query('[:find ?n . :where [?e :community/name "Foo"] [?c :community/neighborhood ?n] [?n :neighborhood/name "Alki"]]', conn.db())
        def newEntity = """
            [[:fns/replace-entity $communityId {
              :community/name "Foo"
              :community/url "http://replaced.example.com"
              :community/neighborhood $neighborhoodId
              :community/category ["Replaced"]
              :community/orgtype :community.orgtype/community
              :community/type :community.type/email-list
            }]]
        """
        conn.transact(Util.readAll(new StringReader(newEntity)).get(0)).get()

def e = conn.db().entity(communityId)
e.keySet().each { key ->
    println "$key = ${e.get(key)}"
}

        assert fooCommunityUrl(conn.db()) == 'http://replaced.example.com'
        def queryForCategory = '[:find [?c ...] :where [?e :community/name "Foo"] [?e :community/category ?c]]'
        assert Peer.query(queryForCategory, conn.db()) as Set == ['Replaced'] as Set
    }

    private static void retractEntity(conn) {
        assert countFooCommunity(conn.db()) == 1
        def communityId = Peer.query('[:find ?e . :where [?e :community/name "Foo"]]', conn.db())

        def retract = "[[:db.fn/retractEntity $communityId]]"
        conn.transact(Util.read(retract)).get()
        assert countFooCommunity(conn.db()) == 0
    }

}
