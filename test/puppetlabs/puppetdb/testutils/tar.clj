(ns puppetlabs.puppetdb.testutils.tar
  (:require [puppetlabs.puppetdb.archive :as archive]
            [clojure.string :as str]
            [clj-time.core :as time]
            [puppetlabs.puppetdb.cheshire :as json]
            [me.raynes.fs :as fs]
            [puppetlabs.puppetdb.utils :as utils]))

(defn path
  "Creates a platform independent relative path built
   from `path-segments`"
  [& path-segments]
  (str/join java.io.File/separator path-segments))

(defn assoc-metadata
  "Creates an export/import metadata map with the current
   time."
  [tar-map]
  (assoc tar-map
    (path "puppetdb-bak" "export-metadata.json")
    {"timestamp" (time/now)
     "command_versions" {"replace_facts" 4
                         "replace_catalog" 6
                         "store_report" 5}}))

(defn collapse-tar-map
  "Collapses the nested map structured created by mapify into
   relative paths, expected by add-entry. Also adds the puppetdb-bak
   root directory and extensions to all paths"
  [m]
  (reduce-kv (fn [top-level dir file-map]
               (merge top-level
                      (reduce-kv (fn [acc filename contents]
                                   (assoc acc (path utils/export-root-dir dir (str filename ".json")) contents))
                                 {} file-map)))
             {} m))

(defn spit-tar
  "Given a `tar-map` created by something like mapify, create a new
   tarball representing that tar-map `f`"
  [f tar-map]
  (let [collapsed-map (assoc-metadata (collapse-tar-map tar-map))]
    (with-open [tar-writer (archive/tarball-writer f)]
      (doseq [[path clj-contents] collapsed-map]
        (archive/add-entry tar-writer "UTF-8" path (json/generate-pretty-string clj-contents))))))

(defn tar-entry->map-path
  [tar-entry]
  (-> (.getName tar-entry) fs/split rest vec))

(defn tar->map
  "Convert elements in an import/export tarball to a hashmap.
   Nested directories convert to nested maps with the files
   converted from JSON to clojure data structures"
  [tar-file]
  (with-open [tar-reader (archive/tarball-reader tar-file)]
    (reduce (fn [acc tar-entry]
              (assoc-in acc
                        (tar-entry->map-path tar-entry)
                        (utils/read-json-content tar-reader)))
            {} (archive/all-entries tar-reader))))
