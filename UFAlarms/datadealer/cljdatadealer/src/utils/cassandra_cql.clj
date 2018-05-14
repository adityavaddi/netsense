;; Copyright (c) 2012-2014 Michael S. Klishin, Alex Petrov, and the ClojureWerkz Team
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns
  utils.cassandra-cql
  (:require
    [clojure.tools.logging :refer :all]
    [utils.config :as conf]
    [utils.cassandra-conversion :as conv]
    [qbits.hayt :as hayt]
    [qbits.hayt.cql :as cql]
    [qbits.hayt.dsl.statement :as statement])
  (:import
    [com.datastax.driver.core Session Statement SimpleStatement PreparedStatement ResultSet ResultSetFuture Cluster PoolingOptions HostDistance Cluster$Builder]
    [com.datastax.driver.core.policies ExponentialReconnectionPolicy RoundRobinPolicy ReconnectionPolicy RetryPolicy DefaultRetryPolicy]
    [com.datastax.driver.core.utils UUIDs]
    [java.util UUID]))


(def cassandra-options (conf/cassservice))

(defn get-cluster
  [{:keys [host max-connections-per-host base-delay-ms max-delay-ms] :or {host ["127.0.0.1"], max-connections-per-host 100 base-delay-ms 200 max-delay-ms 1000}}]
  (let [reconnect-policy (ExponentialReconnectionPolicy. base-delay-ms max-delay-ms)
        retry-policy DefaultRetryPolicy/INSTANCE
        lb-policy (RoundRobinPolicy.)
        ^PoolingOptions poolingOptions (PoolingOptions.)
        ^Cluster$Builder builder (Cluster/builder)
        port 9042
        ]
    (.setMaxConnectionsPerHost poolingOptions HostDistance/LOCAL max-connections-per-host)
    (.withPoolingOptions builder poolingOptions)
    (doseq [h host]
    (.addContactPoint builder h))
    (.withRetryPolicy builder retry-policy)
    (.withReconnectionPolicy builder reconnect-policy)
    (.withLoadBalancingPolicy builder lb-policy)
    (.withPort builder port)
    (.build builder)))

(defn establish-connection
  []
  (.connect (get-cluster cassandra-options)))

(def keyspace (:keyspace cassandra-options))

(def select-query statement/select)

(def use-keyspace-query  statement/use-keyspace)

(def create-keyspace-query statement/create-keyspace)

(def drop-keyspace-query statement/drop-keyspace)

(def delete-query statement/delete)

(defn insert-query
  "http://cassandra.apache.org/doc/cql3/CQL.html#insertStmt
Takes a table identifier and additional clause arguments:
* values
* using"
  [table values & clauses]
  (into {:insert table
         :values values} clauses))

(defn render-query
  "Renders compiled query"
  [query-params]
  (let [renderer hayt/->raw]
    (renderer query-params)))

(defn- build-statement
  "Builds a Prepare or Simple statement out of given params.
   Arities:
     * query + args - for building prepared statements, `query` is a string with placeholders, `values`
       are values to be bound to the built statement for execution.
     * query - for building simple, not prepared statements."
  ([^PreparedStatement query values]
   (.bind query (to-array values)))
  ([^String string-query]
   (SimpleStatement. string-query)))

(defn ^PreparedStatement prepare
  "Prepares the provided query on C* server for futher execution.
   This assumes that query is valid. Returns the prepared statement corresponding to the query."
  ([^Session session ^String query]
   (.prepare ^Session session query)))

(defn compile-query
  "Compiles query from given `builder` and `query-params`"
  [query-params builder]
  (apply builder (flatten query-params)))

(defn- ^Statement statement-for
  [^Session session query prepared?]
  (if prepared?
    (if (coll? query)
      (build-statement (prepare session (first query))
                       (second query))
      (throw (IllegalArgumentException.
               "Query is meant to be executed as prepared, but no values were supplied.")))
    (build-statement query)))


(defn ^ResultSetFuture execute-async
  "Executes a pre-built query and returns a future.
   Options
     * prepared - whether the query should or should not be executed as prepared, always passed
       explicitly, because `execute` is considered to be a low-level function."
  ([^Session session query]
   (execute-async session query {}))
  ([^Session session query {:keys [prepared]}]
   (let [^Statement statement (statement-for session query prepared)
         ^ResultSetFuture fut (.executeAsync session statement)]
     (future (conv/to-clj (.getUninterruptibly fut))))))

(defn execute
  "Executes a pre-built query.
   Options
     * prepared - whether the query should or should not be executed as prepared, always passed
       explicitly, because `execute` is considered to be a low-level function."
  ([^Session session query]
   (execute session query {}))
  ([^Session session query {:keys [prepared fetch-size]}]
   (let [^Statement statement (statement-for session query prepared)
         _                    (when fetch-size
                                (.setFetchSize statement fetch-size))
         ^ResultSetFuture fut (.executeAsync session statement)
         res                  (.getUninterruptibly fut)]
     (conv/to-clj res))))

(defn ^long unix-timestamp
  "Return the unix timestamp contained by the provided time-based UUID."
  [^UUID uuid]
  (UUIDs/unixTimestamp uuid))

(defn ^:private execute-
  [^Session session query-params builder]
  (let [rendered-query (render-query (compile-query query-params builder))]
    (execute session rendered-query {})))

(defn ^:private execute-async-
  [^Session session query-params builder]
  (let [rendered-query (render-query (compile-query query-params builder))]
    (execute-async session rendered-query {})))

(defn use-keyspace
  "Takes an existing keyspace name as argument and set it as the per-session current working keyspace.
   All subsequent keyspace-specific actions will be performed in the context of the selected keyspace,
   unless otherwise specified, until another USE statement is issued or the connection terminates."
  [^Session session ks]
  (execute- session [ks] use-keyspace-query))

(defn create-keyspace
  "Creates a new top-level keyspace. A keyspace is a namespace that
   defines a replication strategy and some options for a set of tables,
   similar to a database in relational databases.
   Example:
     (create-keyspace conn :new_cql_keyspace
                   (with {:replication
                          {:class \"SimpleStrategy\"
                           :replication_factor 1}}))"
  [^Session session & query-params]
  (execute- session query-params create-keyspace-query))

(defn drop-keyspace
  "Drops a keyspace: results in immediate, irreversible removal of an existing keyspace,
   including all column families in it, and all data contained in those column families."
  [^Session session ks]
  (execute- session [ks] drop-keyspace-query))


(defn select
  "Retrieves one or more columns for one or more rows in a table.
   It returns a result set, where every row is a collection of columns returned by the query."
  [^Session session & query-params]
  (execute- session query-params  select-query))

(defn insert
  "Inserts a row in a table.
   Note that since a row is identified by its primary key, the columns that compose it must be
   specified. Also, since a row only exists when it contains one value for a column not part of
   the primary key, one such value must be specified too."
  [^Session session & query-params]
  (execute- session query-params insert-query))

(defn insert-async
  "Same as insert but returns a future"
  [^Session session & query-params]
  (execute-async- session query-params insert-query))

(defn perform-count
  "Helper function to perform count on a table with given query. Count queries are slow in Cassandra,
   in order to get a rough idea of how many items you have in certain table, use `nodetool cfstats`,
   for more complex cases, you can wither do a full table scan or perform a count with this function,
   please note that it does not have any performance guarantees and is potentially expensive."
  [^Session session table & query-params]
  (:count
    (first
     (select session table
             (cons
              (hayt/columns (hayt/count*))
              query-params)))))

(defn delete-async
  "Same as delete but returns a future"
  [^Session session table & query-params]
  (execute-async- session (cons table query-params) delete-query))

(defn create_keyspaces
  "Create the keyspaces required by the system"
  [session key_space class factor]
  (try
    (create-keyspace session key_space
                         (hayt/with {:replication {:class class :replication_factor factor}}))
    (catch Exception e
      (spy :error (ex-info (format "Exception creating keyspace %s" key_space)
                           {:message  (.getMessage e)
                            :cause    (.toString (.getCause e))}))))
  key_space)