(ns puppetlabs.puppetdb.pql
  (:require [puppetlabs.puppetdb.pql.parser :refer [parse]]
            [puppetlabs.puppetdb.pql.transform :refer [transform]]))

(defn pql->ast
  [pql]
  (transform (parse pql)))
