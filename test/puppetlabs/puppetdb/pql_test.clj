(ns puppetlabs.puppetdb.pql-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql :refer :all]))

(deftest test-pql->ast
  (is (= (pql->ast "a == 'a'")
         ["==" "a" "a"])))
