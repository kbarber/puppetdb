(ns puppetlabs.puppetdb.pql
  (:require [instaparse.core :as insta]))

(def parse
  (insta/parser
   (clojure.java.io/resource "puppetlabs/puppetdb/pql.bnf")))
