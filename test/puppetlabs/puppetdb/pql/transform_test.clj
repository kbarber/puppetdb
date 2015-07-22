(ns puppetlabs.puppetdb.pql.transform-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql.transform :refer :all]))

(deftest test-not
  (is (= (transform [:not]) :not)))

(deftest test-transform-condexpression
  (is (= (transform-condexpression "a" "==" 1) ["==" "a" 1])))
(deftest test-condexpression
  (is (= (transform [:condexpression "a" "==" [:integer "1"]]) ["==" "a" 1])))

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
