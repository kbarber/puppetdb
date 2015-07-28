(ns puppetlabs.puppetdb.pql-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql :refer :all]))

(deftest test-pql->ast
  (is (= (pql->ast "a == 'a'")
         ["==" "a" "a"]))
  (is (= (pql->ast "a == 1 or b == 2")
         ["or" ["==" "a" 1] ["==" "b" 2]]))
  (is (= (pql->ast "a == 1 and b == 2")
         ["and" ["==" "a" 1] ["==" "b" 2]]))
  (is (= (pql->ast "!a == 1")
         ["not" ["==" "a" 1]]))
  (is (= (pql->ast "!(a == 1)")
         ["not" ["==" "a" 1]]))
  (is (= (pql->ast "a == 1 or b == 2 and c == 3")
         ["or"
          ["==" "a" 1]
          ["and"
           ["==" "b" 2]
           ["==" "c" 3]]]))
  (is (= (pql->ast "a == 1 or b == 2 and c == 3 or d == 4")
         ["or"
          ["==" "a" 1]
          ["and"
           ["==" "b" 2]
           ["==" "c" 3]]
          ["==" "d" 4]]))
  (is (= (pql->ast "a == 1 or b == 2 and (c == 3 or d == 4)")
         ["or"
          ["==" "a" 1]
          ["and"
           ["==" "b" 2]
           ["or"
            ["==" "c" 3]
            ["==" "d" 4]]]]))
  (is (= (pql->ast "a == 1 or b == 2 and !(c == 3 or d == 4)")
         ["or"
          ["==" "a" 1]
          ["and"
           ["==" "b" 2]
           ["not"
            ["or"
             ["==" "c" 3]
             ["==" "d" 4]]]]]))
    (is (= (pql->ast "a == 1 or b == 2 and (!c == 3 or d == 4)")
         ["or"
          ["==" "a" 1]
          ["and"
           ["==" "b" 2]
           ["or"
            ["not"
             ["==" "c" 3]]
            ["==" "d" 4]]]])))
