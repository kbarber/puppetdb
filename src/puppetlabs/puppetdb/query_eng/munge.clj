(ns puppetlabs.puppetdb.query-eng.munge
  "This part of the query engine focuses on formatting results after the query
  has been made on the database."
  (:require [clojure.tools.logging :as log]
            [clojure.walk :refer [keywordize-keys]]
            [puppetlabs.puppetdb.schema :as pls]
            [schema.core :as s]))

(defn f-to-int
  [f]
  (Integer. (clojure.string/replace-first f "f" "")))

(pls/defn-validated row-to-json-compare
  [a b]
  (compare (f-to-int a) (f-to-int b)))

(pls/defn-validated sort-f-json
  [hmap]
  (sort row-to-json-compare
        hmap))

(pls/defn-validated convert-json
  [value agg-fields]
  value)

(pls/defn-validated reduce-col
  [row]
  (fn [data key config]
    (let [row-value (key row)]
      (case (:type config)
        :json (assoc data key (convert-json row-value (:agg-fields config)))

        (:string :number)
        (assoc data key row-value)))))

(pls/defn-validated column-munges
  [row plan]
  (let [projections (keywordize-keys (:projections plan))]
    (log/spy (reduce-kv (reduce-col row) {} projections))
    (log/spy row)))

(pls/defn-validated standard-munge
  "Provide standard query-eng `munging`."
  [version plan paging-options]
  (fn [rows]
    (map #(column-munges % plan)
         rows)))
