(ns puppetlabs.puppetdb.pql.sql-transform-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql.sql :refer :all]))

(deftest test-transform-from
  (is (= (transform-from "nodes") ["from" "nodes"]))
  (is (= (transform-from "nodes" ["=" "a" 1])
         ["from" "nodes"
          ["=" "a" 1]]))
  (is (= (transform-from ["extract" ["a" "b" "c"]] "nodes")
         ["from" "nodes"
          ["extract" ["a" "b" "c"]]]))
  (is (= (transform-from ["extract" ["a" "b" "c"]] "nodes" ["=" "a" 1])
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["=" "a" 1]]])))
(deftest test-from
  (is (= (transform [:from "nodes"]) ["from" "nodes"]))
  (is (= (transform [:from "nodes"
                     [:expr4
                      [:expr3
                       [:expr2
                        [:condexpression "a" "=" [:integer "1"]]]]]])
         ["from" "nodes"
          ["=" "a" 1]]))
  (is (= (transform [:from
                     [:select "a" "b" "c"]
                     "nodes"
                     [:expr4
                      [:expr3
                       [:expr2
                        [:condexpression
                         "a"
                         "in"
                         [:from
                          [:select "a"]
                          "facts"
                          [:expr4
                           [:expr3
                            [:expr2 [:condexpression "b" "=" [:integer "2"]]]]]]]]]]])
         ["from" "nodes"
          ["extract" ["a" "b" "c"]
           ["in" "a"
            ["from" "facts"
             ["extract" ["a"]
              ["=" "b" 2]]]]]])))

(deftest test-tarnsform-select
  (is (= (transform-select "a" "b" "c")
         ["extract" ["a" "b" "c"]])))
(deftest test-select
  (is (= (transform [:select "a" "b" "c"])
         ["extract" ["a" "b" "c"]])))

(deftest test-transform-expr4
  (is (= (transform-expr4 ["=" "a" 1]) ["=" "a" 1]))
  (is (= (transform-expr4 ["=" "a" 1] ["=" "b" 2])
         ["or"
          ["=" "a" 1]
          ["=" "b" 2]])))
(deftest test-expr4
  (is (= (transform [:expr4
                     [:expr3
                      [:expr2
                       [:condexpression
                        "a" "=" [:integer "1"]]]]])
         ["=" "a" 1]))
  (is (= (transform [:expr4
                     [:expr3
                      [:expr2
                       [:condexpression
                        "a" "=" [:integer "1"]]]]
                     [:expr3
                      [:expr2
                       [:condexpression
                        "b" "=" [:integer "2"]]]]])
         ["or"
          ["=" "a" 1]
          ["=" "b" 2]])))

(deftest test-transform-expr3
  (is (= (transform-expr3 ["=" "a" 1]) ["=" "a" 1]))
  (is (= (transform-expr3 ["=" "a" 1] ["=" "b" 2])
         ["and"
          ["=" "a" 1]
          ["=" "b" 2]])))
(deftest test-expr3
  (is (= (transform [:expr3
                     [:expr2
                      [:condexpression
                       "a" "=" [:integer "1"]]]])
         ["=" "a" 1])
      (= (transform [:expr3
                     [:expr2
                      [:condexpression
                       "a" "=" [:integer "1"]]]
                     [:expr2
                      [:condexpression
                       "b" "=" [:integer "2"]]]])
         ["and"
          ["=" "a" 1]
          ["=" "b" 2]])))

(deftest test-transform-expr2
  (is (= (transform-expr2) nil))
  (is (= (transform-expr2 ["=" "a" 1]) ["=" "a" 1]))
  (is (= (transform-expr2 [:not] ["=" "a" 1]) ["not" ["=" "a" 1]]))
  (is (= (transform-expr2 [:not] nil ["=" "a" 1]) ["not" ["=" "a" 1]])))
(deftest test-expr2
  (is (= (transform [:expr2]) nil))
  (is (= (transform [:expr2
                     ["=" "a" 1]])
         ["=" "a" 1]))
  (is (= (transform [:expr2
                     [:not]
                     [:condexpression "a" "=" [:integer 1]]])
         ["not" ["=" "a" 1]]))
  (is (= (transform [:expr2
                     [:not]
                     nil
                     [:condexpression "a" "=" [:integer 1]]])
         ["not" ["=" "a" 1]])))

(deftest test-transform-condexpression
  (is (= (transform-condexpression "a" "~" "foo") ["~" "a" "foo"]))
  (is (= (transform-condexpression "a" "==" 1) ["==" "a" 1])))
(deftest test-condexpression
  (is (= (transform [:condexpression "a" "==" [:integer "1"]]) ["==" "a" 1]))
  (is (= (transform [:condexpression "a" "~" [:regexp "foo"]]) ["~" "a" "foo"])))

(deftest test-transform-groupedfieldlist
  (is (= (transform-groupedfieldlist "a" "b") ["a" "b"]))
  (is (= (transform-groupedfieldlist "a") ["a"])))
(deftest test-groupedfieldlist
  (is (= (transform [:groupedfieldlist "a" "b"]) ["a" "b"]))
  (is (= (transform [:groupedfieldlist "a"]) ["a"])))

(deftest test-transform-regexp
  (is (= (transform-regexp "foo") "foo")))
(deftest test-regexp
  (is (= (transform [:regexp "asdf"]) "asdf")))

(deftest test-transform-string
  (is (= (transform-string "foo") "foo")))
(deftest test-string
  (is (= (transform [:string "asdf"]) "asdf")))

(deftest test-transform-boolean
  (is (= (transform-boolean [:true]) true))
  (is (= (transform-boolean [:false]) false)))
(deftest test-boolean
  (is (= (transform [:boolean [:true]]) true))
  (is (= (transform [:boolean [:false]]) false)))

(deftest test-transform-integer
  (is (= (transform-integer "3") 3))
  (is (= (transform-integer "-" "3") -3)))
(deftest test-integer
  (is (= (transform [:integer "3"]) 3))
  (is (= (transform [:integer "-" "3"]) -3)))

(deftest test-transform-real
  (is (= (transform-real "1" "." "1") 1.1))
  (is (= (transform-real "1" "." "1" "E123") 1.1E123))
  (is (= (transform-real "-" "1" "." "1") -1.1))
  (is (= (transform-real "-" "1" "." "1" "E-123") -1.1E-123)))
(deftest test-tranform-exp
  (is (= (transform-exp "123") "E123"))
  (is (= (transform-exp "-" "123") "E-123")))
(deftest test-real
  (is (= (transform [:real "1" "." "1"]) 1.1))
  (is (= (transform [:real "1" "." "1" [:exp "45"]]) 1.1E45))
  (is (= (transform [:real "1" "." "1" [:exp "-" "45"]]) 1.1E-45))
  (is (= (transform [:real "-" "1" "." "1"]) -1.1)))
