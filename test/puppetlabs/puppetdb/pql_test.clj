(ns puppetlabs.puppetdb.pql-test
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
             ["==" "d" 4]]]]])))
