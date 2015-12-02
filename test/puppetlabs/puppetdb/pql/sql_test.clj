(ns puppetlabs.puppetdb.pql.sql-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql :refer :all]))

(deftest test-pql->ast
  (is (= (pql->ast "from nodes where a == 'a'")
         ["from" "nodes"
          ["==" "a" "a"]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2")
         ["from" "nodes"
          ["or" ["==" "a" 1] ["==" "b" 2]]]))
  (is (= (pql->ast "from nodes where a == 1 and b == 2")
         ["from" "nodes"
          ["and" ["==" "a" 1] ["==" "b" 2]]]))
  (is (= (pql->ast "from nodes where !a == 1")
         ["from" "nodes"
          ["not" ["==" "a" 1]]]))
  (is (= (pql->ast "from nodes where !(a == 1)")
         ["from" "nodes"
          ["not" ["==" "a" 1]]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2 and c == 3")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["==" "c" 3]]]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2 and c == 3 or d == 4")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["==" "c" 3]]
           ["==" "d" 4]]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2 and (c == 3 or d == 4)")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["or"
             ["==" "c" 3]
             ["==" "d" 4]]]]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2 and !(c == 3 or d == 4)")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["not"
             ["or"
              ["==" "c" 3]
              ["==" "d" 4]]]]]]))
  (is (= (pql->ast "from nodes where a == 1 or b == 2 and (!c == 3 or d == 4)")
         ["from" "nodes"
          ["or"
           ["==" "a" 1]
           ["and"
            ["==" "b" 2]
            ["or"
             ["not"
              ["==" "c" 3]]
             ["==" "d" 4]]]]]))
  (is (= (pql->ast "select a, b, c from nodes")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]]]))
  (is (= (pql->ast "select a, b, c from nodes where a = 1")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["=" "a" 1]]]))
  (is (= (pql->ast "select a, b, c from nodes where a in (select x from resources where x = 1)")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["in" "a"
            ["from" "resources"
             ["extract" ["x"]
              ["=" "x" 1]]]]]]))
  (is (= (pql->ast "select a, b, c from nodes where (a, b) in (select a, b from resources where x = 1)")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["in" ["a" "b"]
            ["from" "resources"
             ["extract" ["a" "b"]
              ["=" "x" 1]]]]]]))
  (is (= (pql->ast "select value from facts where (certname,name) in (select certname,name from fact_contents where value < 100)")
         ["from" "facts"
          ["extract" ["value"]
           ["in" ["certname" "name"]
            ["from" "fact_contents"
             ["extract" ["certname" "name"] ["<" "value" 100]]]]]])))
