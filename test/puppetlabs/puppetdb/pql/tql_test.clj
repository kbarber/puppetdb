(ns puppetlabs.puppetdb.pql.tql-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql :refer :all]))

(defn pql
  [query]
  (first (pql->ast2 query)))

;; TODO: this follows the 'are' pattern really, we should switch
(deftest test-pql->ast
  (is (= (pql "nodes { a == 'a' }")
         ["from" "nodes"
          ["==" "a" "a"]]))
  (is (= (pql "nodes { a == 1 or b == 2 }")
         ["from" "nodes"
          ["or" ["==" "a" 1] ["==" "b" 2]]]))
  (is (= (pql "nodes { a == 1 and b == 2 }")
         ["from" "nodes"
          ["and" ["==" "a" 1] ["==" "b" 2]]]))
  (is (= (pql "nodes { !a == 1 }")
         ["from" "nodes"
          ["not" ["==" "a" 1]]]))
  (is (= (pql "nodes { !(a == 1) }")
         ["from" "nodes"
          ["not" ["==" "a" 1]]]))
  (is (= (pql "nodes { a == 1 or b == 2 and c == 3 }")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["==" "c" 3]]]]))
  (is (= (pql "nodes { a == 1 or b == 2 and c == 3 or d == 4 }")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["==" "c" 3]]
           ["==" "d" 4]]]))
  (is (= (pql "nodes { a == 1 or b == 2 and (c == 3 or d == 4) }")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["or"
             ["==" "c" 3]
             ["==" "d" 4]]]]]))
  (is (= (pql "nodes { a == 1 or b == 2 and !(c == 3 or d == 4) }")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["not"
             ["or"
              ["==" "c" 3]
              ["==" "d" 4]]]]]]))
  (is (= (pql "nodes { a == 1 or b == 2 and (!c == 3 or d == 4) }")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["or"
             ["not"
              ["==" "c" 3]]
             ["==" "d" 4]]]]]))
  (is (= (pql "nodes {} [a, b, c]")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]]]))
  (is (= (pql "nodes { a = 1 } [a, b, c]")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["=" "a" 1]]]))
  (is (= (pql "nodes { a in resources { x = 1 } [x] } [a, b, c]")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["in" "a"
            ["from" "resources"
             ["extract" ["x"]
              ["=" "x" 1]]]]]]))
  (is (= (pql "nodes { (a, b) in resources { x = 1 } [a, b] } [a, b, c]")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["in" ["a" "b"]
            ["from" "resources"
             ["extract" ["a" "b"]
              ["=" "x" 1]]]]]]))
  (is (= (pql "facts { (certname,name) in fact_contents { value < 100 }[certname,name]}[value]")
         ["from" "facts"
          ["extract" ["value"]
           ["in" ["certname" "name"]
            ["from" "fact_contents"
             ["extract" ["certname" "name"] ["<" "value" 100]]]]]])))
