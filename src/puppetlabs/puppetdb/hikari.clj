(ns puppetlabs.puppetdb.hikari
  (:import com.zaxxer.hikari.HikariConfig
           com.zaxxer.hikari.HikariDataSource)
  (:require [schema.core :as s]
            [metrics.core :as metrics]))

(def default-datasource-options
  {:auto-commit              false
   :read-only                false
   :initialization-fail-fast false
   :register-mbeans          true})

(defn- gte-0?
  "Returns true if num is greater than or equal 0, else false"
  [x]
  (>= x 0))

(defn- gte-1?
  "Returns true if num is greater than or equal 1, else false"
  [x]
  (>= x 1))

(defn- gte-1000?
  "Returns true if num is greater than or equal 1000, else false"
  [x]
  (>= x 1000))

(def ^{:private true} IntGte0
  (s/both s/Int (s/pred gte-0? 'gte-0?)))

(def ^{:private true} IntGte1
  (s/both s/Int (s/pred gte-1? 'gte-1?)))

(def ^{:private true} IntGte1000
  (s/both s/Int (s/pred gte-1000? 'gte-1000?)))

(def ConfigurationOptions
  {:auto-commit                         s/Bool
   (s/optional-key :connection-timeout) IntGte1000
   (s/optional-key :idle-timeout)       IntGte0
   :initialization-fail-fast            s/Bool
   (s/optional-key :max-lifetime)       IntGte0
   (s/optional-key :maximum-pool-size)  IntGte1
   (s/optional-key :minimum-idle)       IntGte0
   :pool-name                           s/Str
   :read-only                           s/Bool
   :register-mbeans                     s/Bool
   ;; TODO: this is actually an enum, so probably should have validation for the allowed subnames
   :subname                             s/Str
   :subprotocol                         s/Str
   (s/optional-key :validation-timeout) IntGte1000
   s/Keyword                            s/Any})

(defn- exception-message
  ""
  [e]
  (format "Invalid configuration options: %s" (keys (:error (.getData e)))))

(s/defn ^:always-validate add-datasource-property
  "Add a custom datasource property to the underlying database driver."
  [config :- HikariConfig
   property :- s/Str
   value :- s/Str]
  (.addDataSourceProperty config property value))

(s/defn ^:always-validate add-datasource-properties
  "Like add-datasource-property, but allows you to pass a map of properties and
   values."
  [config :- HikariConfig
   properties :- {s/Str s/Str}]
  (doseq [[k v] properties]
    (add-datasource-property config k v)))

(defn validate-options
  ""
  [options]
  (s/validate ConfigurationOptions (merge default-datasource-options options))
  ;; TODO: commenting this out for now so I can see the underlying schema failure properly
  #_(try
    (s/validate ConfigurationOptions (merge default-datasource-options options))
    (catch clojure.lang.ExceptionInfo e
      (throw
       (IllegalArgumentException. (exception-message e))))))

(s/defn ^:always-validate datasource-config :- HikariConfig
  "Produce a valid HikariConfig object from provided configuration options."
  [datasource-options]
  (let [config (HikariConfig.)
        options (validate-options datasource-options)
        {:keys [adapter
                auto-commit
                connection-test-query
                connection-timeout
                idle-timeout
                initialization-fail-fast
                max-lifetime
                maximum-pool-size
                minimum-idle
                password
                pool-name
                read-only
                register-mbeans
                subname
                subprotocol
                username
                validation-timeout]} options]

    ;; Set pool-specific properties
    (doto config
      (.setAutoCommit auto-commit)
      (.setReadOnly read-only)
      (.setInitializationFailFast initialization-fail-fast)
      (.setRegisterMbeans register-mbeans)
      (.setJdbcUrl (str "jdbc:" subprotocol ":" subname))
      ;; Without explicit transaction isolation and a test query, we are unable
      ;; to change the transaction isolation level on a per query basis since
      ;; Connection.isAlive() tests aren't transactionally isolated.
      (.setIsolateInternalQueries true)
      (.setConnectionTestQuery "/* hikaricp test query */ select 1")
      ;; TODO: we're going to need to upgrade to the latest metrics to do this
      #_(.setMetricRegistry (metrics/default-registry)))

    ;; Set optional properties
    (when idle-timeout (.setIdleTimeout config idle-timeout))
    (when max-lifetime (.setMaxLifetime config max-lifetime))
    (when minimum-idle (.setMinimumIdle config minimum-idle))
    (when maximum-pool-size (.setMaximumPoolSize config maximum-pool-size))
    (when validation-timeout (.setValidationTimeout config validation-timeout))
    (when connection-timeout (.setConnectionTimeout config connection-timeout))
    (when username (.setUsername config username))
    (when password (.setPassword config password))
    (when pool-name (.setPoolName config pool-name))
    (when connection-test-query (.setConnectionTestQuery config connection-test-query))

    ;; Set driver specific properties
    (case subprotocol
      "postgresql" (add-datasource-properties config {"ApplicationName" pool-name
                                                      "tcpKeepAlive" "true"
                                                      "logUnclosedConnections" "true"})
      "default")

    config))

(s/defn ^:always-validate make-datasource :- HikariDataSource
  "Create a HikariDataSource from provided configuration."
  [datasource-options]
  (let [config (datasource-config datasource-options)
        datasource (HikariDataSource. config)]
    datasource))

(s/defn ^:always-validate close-datasource
  "Close an open HikariDataSource."
  [datasource :- HikariDataSource]
    (.close datasource))

;; TODO: copied from jdbc.clj, will need to fix this up

(defn make-connection-pool
  [{:keys [classname subprotocol subname user username password
           partition-conn-min partition-conn-max partition-count
           stats log-statements log-slow-statements statements-cache-size
           conn-max-age conn-lifetime conn-keep-alive read-only?
           pool-name]
    :as   db}]
  (let [datasource-options {:read-only          read-only?
                            ;; TODO: shame we can't change all our settings to be milliseconds like hikaricp
                            :max-lifetime       (* 60000 conn-lifetime)
                            ;; TODO: work these out from partition settings?
                            :minimum-idle       8
                            :maximum-pool-size  10
                            :pool-name          pool-name
                            ;; TODO: some consumers use :user versus :username, migratus also relies on :user I think
                            :username           (or user username)
                            :password           password
                            :subprotocol        subprotocol
                            :subname            subname}]
    (make-datasource datasource-options)))

(s/defn ^:always-validate pooled-datasource :- {:datasource HikariDataSource}
  "Given a database connection attribute map, return a JDBC datasource
  compatible with clojure.java.jdbc that is backed by a connection
  pool."
  [options]
  {:datasource (make-connection-pool options)})









;; TODO: this will go
#_(defn make-connection-pool
  "Create a new database connection pool"
  [{:keys [classname subprotocol subname user username password
           partition-conn-min partition-conn-max partition-count
           stats log-statements log-slow-statements statements-cache-size
           conn-max-age conn-lifetime conn-keep-alive read-only?]
    :as   db}]
  (let [;; Load the database driver class explicitly, to avoid jar load ordering
        ;; issues.
        _ (Class/forName classname)
        log-slow-statements-duration (pl-time/to-seconds log-slow-statements)
        config          (doto (new BoneCPConfig)
                          (.setDefaultAutoCommit false)
                          (.setLazyInit true)
                          (.setMinConnectionsPerPartition partition-conn-min)
                          (.setMaxConnectionsPerPartition partition-conn-max)
                          (.setPartitionCount partition-count)
                          (.setConnectionTestStatement "begin; select 1; commit;")
                          (.setStatisticsEnabled stats)
                          (.setIdleMaxAgeInMinutes (pl-time/to-minutes conn-max-age))
                          (.setIdleConnectionTestPeriodInMinutes (pl-time/to-minutes conn-keep-alive))
                          ;; paste the URL back together from parts.
                          (.setJdbcUrl (str "jdbc:" subprotocol ":" subname))
                          (.setConnectionHook (connection-hook log-statements log-slow-statements-duration))
                          (.setStatementsCacheSize statements-cache-size)
                          (.setDefaultReadOnly read-only?))
        user (or user username)]
    ;; configurable without default
    (when user (.setUsername config (str user)))
    (when password (.setPassword config (str password)))
    (when conn-lifetime (.setMaxConnectionAge config (pl-time/to-minutes conn-lifetime) TimeUnit/MINUTES))
    (when log-statements (.setLogStatementsEnabled config log-statements))

    (.setQueryExecuteTimeLimit config log-slow-statements-duration (TimeUnit/SECONDS))
    ;; ...aaand, create the pool.
    (BoneCPDataSource. config)))
