(ns puppetlabs.puppetdb.pql
  (:require [clojure.string :refer [join]]
            [clojure.tools.logging :as log]
            [instaparse.print :as print]
            [instaparse.failure :as failure]
            [puppetlabs.puppetdb.pql.sql :as sql]
            [puppetlabs.puppetdb.pql.tql :as tql]))

(defn print-reason
  "Provides special case for printing negative lookahead reasons"
  [r]
  (cond
    (:NOT r)
    (format "NOT %s" (:NOT r))
    (:char-range r)
    (print/char-range->str r)
    (instance? java.util.regex.Pattern r)
    (print/regexp->str r)
    :else
    r))

(defn pprint-failure
  "Takes an augmented failure object and prints the error message"
  [{:keys [line column text reason]}]
  (let [opening (format "PQL parse error at line %d, column %d:\n\n%s\n%s\n\n"
                        line column text (failure/marker column))
        full-reasons (distinct (map :expecting
                                    (filter :full reason)))
        partial-reasons (distinct (map :expecting
                                       (filter (complement :full) reason)))
        total (+ (count full-reasons) (count partial-reasons))
        expected (cond (zero? total) nil
                       (= 1 total) "Expected:\n"
                       :else "Expected one of:\n\n")
        freasons (join (for [r full-reasons]
                        (join [(print-reason r)
                               " (followed by end-of-string)"])))
        preasons (join
                  (for [r partial-reasons]
                    (join [(print-reason r) "\n"])))]
    (join [opening expected freasons preasons])))

(defn pql->ast
  [pql]
  (first (sql/transform (sql/parse pql))))

(defn pql->ast2
  [pql]
  (log/spy (tql/transform (log/spy (tql/parse pql)))))
