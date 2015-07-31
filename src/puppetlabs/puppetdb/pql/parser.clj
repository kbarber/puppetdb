(ns puppetlabs.puppetdb.pql.parser
  (:require [instaparse.core :as insta]))

(def parse
  (insta/parser
   (clojure.java.io/resource "puppetlabs/puppetdb/pql.ebnf")))
