(ns puppetlabs.puppetdb.pql
  (:require [clojure.tools.logging :as log]
            [puppetlabs.puppetdb.pql.sql :as sql]
            [puppetlabs.puppetdb.pql.tql :as tql]))

(defn pql->ast
  [pql]
  (first (sql/transform (sql/parse pql))))

(defn pql->ast2
  [pql]
  (log/spy (tql/transform (log/spy (tql/parse pql)))))
