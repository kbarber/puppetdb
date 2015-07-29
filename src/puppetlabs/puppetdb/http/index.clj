(ns puppetlabs.puppetdb.http.index
  (:require [clojure.tools.logging :as log]
            [net.cgrand.moustache :refer [app]]
            [puppetlabs.puppetdb.http :as http]
            [puppetlabs.puppetdb.http.query :as http-q]
            [puppetlabs.puppetdb.jdbc :refer [with-transacted-connection]]
            [puppetlabs.puppetdb.middleware :refer [verify-accepts-json validate-query-params
                                                    wrap-with-paging-options]]
            [puppetlabs.puppetdb.pql :refer [pql->ast]]
            [puppetlabs.puppetdb.query-eng :refer [produce-streaming-body]]
            [puppetlabs.puppetdb.query.paging :as paging]))

(defn build-index-app
  [version]
  (fn [{:keys [params globals paging-options]}]
    (let [query  (log/spy (pql->ast (log/spy (params "query"))))
          entity (keyword (get query 1))
          query  (get query 2)]
      (produce-streaming-body
       entity
       version
       query
       paging-options
       (:scf-read-db globals)
       (:url-prefix globals)))))

(defn routes
  [version]
  (app
   [""]
   {:get (build-index-app version)}))

(defn index-app
  [version]
  (-> (routes version)
      verify-accepts-json
      (validate-query-params
       {:required ["query"]
        :optional paging/query-params})
      wrap-with-paging-options))
