(ns puppetlabs.puppetdb.hikari
  (:import com.zaxxer.hikari.HikariConfig
           com.zaxxer.hikari.HikariDataSource)
  (:require [org.tobereplaced.lettercase :refer [mixed-name]]
            [schema.core :as s]))

(def default-datasource-options
  {:auto-commit        false
   :read-only          false
   :initialization-fail-fast false
   :register-mbeans    true})

(def ^{:private true} adapters-to-datasource-class-names
  {"derby"          "org.apache.derby.jdbc.ClientDataSource"
   "firebird"       "org.firebirdsql.pool.FBSimpleDataSource"
   "db2"            "com.ibm.db2.jcc.DB2SimpleDataSource"
   "h2"             "org.h2.jdbcx.JdbcDataSource"
   "hsqldb"         "org.hsqldb.jdbc.JDBCDataSource"
   "mariadb"        "org.mariadb.jdbc.MySQLDataSource"
   "mysql"          "com.mysql.jdbc.jdbc2.optional.MysqlDataSource"
   "sqlserver-jtds" "net.sourceforge.jtds.jdbcx.JtdsDataSource"
   "sqlserver"      "com.microsoft.sqlserver.jdbc.SQLServerDataSource"
   "oracle"         "oracle.jdbc.pool.OracleDataSource"
   "pgjdbc-ng"      "com.impossibl.postgres.jdbc.PGDataSource"
   "postgresql"     "org.postgresql.ds.PGSimpleDataSource"
   "fdbsql"         "com.foundationdb.sql.jdbc.ds.FDBSimpleDataSource"
   "sybase"         "com.sybase.jdbcx.SybDataSource"})

(def ^{:private true} AdaptersList
  (apply s/enum (keys adapters-to-datasource-class-names)))

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
  {#_#_:adapter            AdaptersList
   :auto-commit        s/Bool
   (s/optional-key :connection-timeout) IntGte1000
   (s/optional-key :idle-timeout)       IntGte0
   :initialization-fail-fast s/Bool
   :jdbc-url           s/Str
   (s/optional-key :max-lifetime)       IntGte0
   (s/optional-key :maximum-pool-size)  IntGte1
   (s/optional-key :minimum-idle)       IntGte0
   ;; TODO: maybe we just work this out from app-name?
   :pool-name          s/Str
   :read-only          s/Bool
   :register-mbeans    s/Bool
   (s/optional-key :validation-timeout) IntGte1000
   s/Keyword           s/Any})

(defn- exception-message
  ""
  [e]
  (format "Invalid configuration options: %s" (keys (:error (.getData e)))))

(defn- add-datasource-property
  ""
  [config property value]
  (if (not (nil? value)) (.addDataSourceProperty config (mixed-name property) value)))

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

(defn datasource-config
  ""
  [datasource-options]
  (let [config (HikariConfig.)
        options               (validate-options datasource-options)
        not-core-options      (apply dissoc options
                                     (conj (keys ConfigurationOptions)
                                           :username :password :pool-name :connection-test-query))
        {:keys [adapter
                auto-commit
                connection-test-query
                connection-timeout
                idle-timeout
                initialization-fail-fast
                jdbc-url
                max-lifetime
                maximum-pool-size
                minimum-idle
                password
                pool-name
                read-only
                register-mbeans
                username
                validation-timeout]} options
        datasource-class-name (get
                               adapters-to-datasource-class-names
                               adapter)]
    ;; Set pool-specific properties
    (doto config
      (.setAutoCommit          auto-commit)
      (.setReadOnly            read-only)
      #_(.setDataSourceClassName datasource-class-name)
      (.setInitializationFailFast initialization-fail-fast)
      (.setRegisterMbeans      register-mbeans))
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
    (when jdbc-url (.setJdbcUrl config jdbc-url))
    ;; Set datasource-specific properties
    (doseq [[k v] not-core-options]
      (add-datasource-property config k v))
    config))

(defn make-datasource
  ""
  [datasource-options]
  (let [config (datasource-config datasource-options)
        datasource (HikariDataSource. config)]
    datasource))

(defn close-datasource
  ""
  [datasource]
    (.close datasource))
