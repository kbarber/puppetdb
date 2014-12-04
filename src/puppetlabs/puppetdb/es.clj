(ns puppetlabs.puppetdb.es
  "Elastic search backend handling"
  (:require [clojurewerkz.elastisch.rest :as rest]
            [clojure.tools.logging :as log]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.rest.index :as esi]))

(def ^{:private true} puppetdb-mapping-types
  {"reports" {:properties {:product {:type "string"}
                           :version {:type "string"}
                           :ip      {:type "string"}
                           :params  {:type "object"}}
              :_timestamp {:enabled true
                           :format  "YYYY-MM-dd'T'HH:mm"}}})

(defn connect
  []
  (rest/connect "http://127.0.0.1:9200"))

(defn drop-index
  [conn index]
  (esi/delete conn index))

(defn ensure-index
  [conn index mapping]
  (when-not (esi/exists? conn index)
    (esi/create conn index mapping)))

(defn store-report
  [report]
  (let [conn (connect)
        index "puppetdb"]
    (ensure-index conn index puppetdb-mapping-types)
    (let [id (esd/create conn index "reports" report)]
      (log/info (str "Elasticsearch id: " id)))))
