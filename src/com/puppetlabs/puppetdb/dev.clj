(ns com.puppetlabs.puppetdb.dev
  (:require [com.puppetlabs.puppetdb.schema :as pls]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as sql]
            [clojure.string :as str]))

(def trace-sql-status (atom false))

(def pretty-sql-formatter (atom nil))

(pls/defn-validated trace-sql-on
  "Toggles pretty-sql status on."
  []
  (let [formatter (.newInstance (Class/forName "org.hibernate.engine.jdbc.internal.BasicFormatterImpl"))]
    (do
      (swap! pretty-sql-formatter (constantly formatter))
      (swap! trace-sql-status (constantly true)))))

(pls/defn-validated trace-sql-off
  "Toggles pretty-sql status off."
  []
  (swap! trace-sql-status (constantly false)))

(pls/defn-validated trace-sql
  "Log pretty SQL in a pretty way. Return the original SQL."
  [sql :- s/Str
   params :- (s/maybe [s/Any])]
  (when @trace-sql-status
    (sql/with-query-results rs
      (apply vector (str "EXPLAIN (VERBOSE, ANALYZE, BUFFERS) " sql) params)
      (log/info
       (str "Formatted SQL:\n"
            (.format @pretty-sql-formatter sql)
            "\n"))
      (log/info
       (str "Explain plan:\n\n"
            (str/join "\n" (map #(get % (keyword "query plan")) rs))
            "\n")))))
