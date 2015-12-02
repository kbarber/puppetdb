(ns puppetlabs.puppetdb.pql.sql-parser-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [puppetlabs.puppetdb.pql.sql :refer [parse]]))

;; These tests are ordered the same as in the EBNF file, so one can
;; develop the expressions and tests side-by-side.

(deftest test-query
  (is (= (parse "from nodes" :start :query)
         [[:from "nodes"]])))

(deftest test-from
  (is (= (parse "from nodes" :start :from)
         [:from "nodes"]))
  (is (= (parse "from nodes where a == 1" :start :from)
         [:from
          "nodes"
          [:expr4
           [:expr3
            [:expr2
             [:condexpression "a" "==" [:integer "1"]]]]]]))
  (is (= (parse "select a,b,c from nodes" :start :from)
         [:from
          [:select "a" "b" "c"]
          "nodes"]))
  (is (= (parse "select a,b,c from nodes where a == 1" :start :from)
         [:from
          [:select "a" "b" "c"]
          "nodes"
          [:expr4
           [:expr3
            [:expr2
             [:condexpression "a" "==" [:integer "1"]]]]]]))
  (is (= (parse "select a,b,c from nodes where a in (select a from facts where b = 2)")
         [[:from
           [:select "a" "b" "c"]
           "nodes"
           [:expr4
            [:expr3
             [:expr2
              [:condexpression "a" "in"
               [:from
                [:select "a"]
                "facts"
                [:expr4
                 [:expr3
                  [:expr2
                   [:condexpression "b" "=" [:integer "2"]]]]]]]]]]]]))
  (is (= (parse "select a,b,c from nodes where (a,b) in (select a,b from facts where c = 3)")
         [[:from
           [:select "a" "b" "c"]
           "nodes"
           [:expr4
            [:expr3
             [:expr2
              [:condexpression
               [:groupedfieldlist "a" "b"]
               "in"
               [:from
                [:select "a" "b"]
                "facts"
                [:expr4
                 [:expr3
                  [:expr2
                   [:condexpression "c" "=" [:integer "3"]]]]]]]]]]]]))
  (is (= (parse "select value from facts where (certname,name) in (select certname,name from fact_contents where value < 100)")
         [[:from
            [:select "value"]
            "facts"
            [:expr4
             [:expr3
              [:expr2
               [:condexpression
                [:groupedfieldlist "certname" "name"]
                "in"
                [:from
                 [:select "certname" "name"]
                 "fact_contents"
                 [:expr4
                  [:expr3
                   [:expr2 [:condexpression "value" "<" [:integer "100"]]]]]]]]]]]])))

(deftest test-entity
  (is (= (parse "nodes" :start :entity)
         ["nodes"])))

(deftest test-select
  (is (= (parse "select a,b,c" :start :select)
         [:select "a" "b" "c"])))

(deftest test-selectfields
  (is (= (parse "a" :start :selectfields)
         ["a"]))
  (is (= (parse "a, b" :start :selectfields)
         ["a" "b"]))
  (is (= (parse "a,b,c" :start :selectfields)
         ["a" "b" "c"])))

(deftest test-where
  (is (= (parse "where a == 1" :start :where)
         [[:expr4
            [:expr3
             [:expr2
              [:condexpression "a" "==" [:integer "1"]]]]]])))

(deftest test-expressions
  (is (= (parse "a == 1" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:condexpression "a" "==" [:integer "1"]]]]]]))
  (is (= (parse "!a == 1" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:not]
             [:expr2
              [:condexpression "a" "==" [:integer "1"]]]]]]]))
  (is (= (parse "!(a == 1)" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:not]
             [:expr2]
             [:expr2
              [:expr4
               [:expr3
                [:expr2
                 [:condexpression "a" "==" [:integer "1"]]]]]]]]]]))
  (is (= (parse "a == 1 and b == 2" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:condexpression "a" "==" [:integer "1"]]]
            [:expr3
             [:expr2
              [:condexpression "b" "==" [:integer "2"]]]]]]]))
  (is (= (parse "c == 3 or d == 4 and a == 1" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:condexpression "c" "==" [:integer "3"]]]]
           [:expr4
            [:expr3
             [:expr2
              [:condexpression "d" "==" [:integer "4"]]]
             [:expr3
              [:expr2
               [:condexpression "a" "==" [:integer "1"]]]]]]]]))
  (is (= (parse "c == 3 or d == 4 and a == 1 or b == 2" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:condexpression "c" "==" [:integer "3"]]]]
           [:expr4
            [:expr3
             [:expr2
              [:condexpression "d" "==" [:integer "4"]]]
             [:expr3
              [:expr2
               [:condexpression "a" "==" [:integer "1"]]]]]]
           [:expr4
            [:expr3
             [:expr2
              [:condexpression "b" "==" [:integer "2"]]]]]]]))
  (is (= (parse "(c == 3 or d == 4) and (a == 1 or b == 2)" :start :expression)
         [[:expr4
           [:expr3
            [:expr2
             [:expr4
              [:expr3
               [:expr2
                [:condexpression "c" "==" [:integer "3"]]]]
              [:expr4
               [:expr3
                [:expr2
                 [:condexpression "d" "==" [:integer "4"]]]]]]]
            [:expr3
             [:expr2
              [:expr4
               [:expr3
                [:expr2
                 [:condexpression "a" "==" [:integer "1"]]]]
               [:expr4
                [:expr3
                 [:expr2
                  [:condexpression "b" "==" [:integer "2"]]]]]]]]]]]))

  (is (insta/failure? (insta/parse parse "foo and 'bar'" :start :expression))))

(deftest test-condexpression
  (testing "condexpression"
    (is (= (parse "certname = 'foobar'" :start :condexpression)
           [:condexpression "certname" "=" [:string "foobar"]]))
    (is (= (parse "certname == 'foobar'" :start :condexpression)
           [:condexpression "certname" "==" [:string "foobar"]]))
    (is (= (parse "certname =~ /foobar/" :start :condexpression)
           [:condexpression "certname" "=~" [:regexp "foobar"]]))
    (is (= (parse "certname > 4" :start :condexpression)
           [:condexpression "certname" ">" [:integer "4"]]))

    (is (insta/failure? (insta/parse parse "foo >= 'bar'" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo >= true" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo <= 'bar'" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo <= true" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo > 'bar'" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo < true" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo =~ 'bar'" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo !~ 'bar'" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo == bar" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "foo != bar" :start :condexpression)))
    (is (insta/failure? (insta/parse parse "'foo' == bar" :start :condexpression))))

  (testing "condexpregexp"
    (is (= (parse "a =~ /asdf/" :start :condexpregexp) ["a" "=~" [:regexp "asdf"]]))
    ;; TODO: there is no concept of !~ in our language today
    #_(is (= (parse "a !~ /asdf/" :start :condexpregexp) ["a" "!~" [:regexp "asdf"]]))

    (is (insta/failure? (insta/parse parse "a =~ 'bar'" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "a =~ 4" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "a =~ true" :start :condexpregexp)))
    #_(is (insta/failure? (insta/parse parse "a !~ 'bar'" :start :condexpregexp))))

  (testing "condexpnumber"
    (is (= (parse "a >= 4" :start :condexpnumber) ["a" ">=" [:integer "4"]]))

    (is (insta/failure? (insta/parse parse "a >= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "a >= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "a <= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "a <= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "a > 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "a < true" :start :condexpnumber))))

  (testing "condexpmatch"
    (is (= (parse "a == 'bar'" :start :condexpmatch) ["a" "==" [:string "bar"]]))

    (is (insta/failure? (insta/parse parse "a == bar" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "a == /bar/" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "a != bar" :start :condexpmatch))))

  (testing "condexpsubquery"
    (is (= (parse "a in (select a from nodes)" :start :condexpsubquery)
           ["a" "in" [:from [:select "a"] "nodes"]]))
    (is (= (parse "(a, b) in (select a, b from nodes)" :start :condexpsubquery)
           [[:groupedfieldlist "a" "b"] "in" [:from [:select "a" "b"] "nodes"]]))
    (is (= (parse "(a) in (select a, b from nodes)" :start :condexpsubquery)
           [[:groupedfieldlist "a"] "in" [:from [:select "a" "b"] "nodes"]]))

    (is (insta/failure? (insta/parse parse "a,b in (select a, b from nodes)")))))

(deftest test-conditionalexpressionparts
  (testing "groupedfieldlist"
    (is (= (parse "(value,certname)" :start :groupedfieldlist)
           [:groupedfieldlist "value", "certname"]))
    (is (= (parse "( value , certname )" :start :groupedfieldlist)
           [:groupedfieldlist "value", "certname"]))

    (is (insta/failure? (insta/parse parse "value, certname"))))

  (testing "fieldlist"
    (is (= (parse "value, certname" :start :fieldlist) ["value", "certname"]))
    (is (= (parse "foo,var" :start :fieldlist) ["foo", "var"]))

    (is (insta/failure? (insta/parse parse "foo:var"))))

  (testing "field"
    (is (= (parse "certname" :start :field) ["certname"]))
    (is (= (parse "value" :start :field) ["value"]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :field))))

  (testing "condregexp"
    (is (= (parse "=~" :start :condregexp) ["=~"]))
    ;; TODO: there is no concept of !~ in our language today
    #_(is (= (parse "!~" :start :condregexp) ["!~"]))

    (is (insta/failure? (insta/parse parse "=" :start :condregexp)))
    (is (insta/failure? (insta/parse parse "==" :start :condregexp))))

  (testing "condnumber"
    (is (= (parse ">=" :start :condnumber) [">="]))
    (is (= (parse "<=" :start :condnumber) ["<="]))

    (is (insta/failure? (insta/parse parse "=" :start :condnumber)))
    (is (insta/failure? (insta/parse parse "==" :start :condnumber))))

  (testing "condmatch"
    (is (= (parse "==" :start :condmatch) ["=="]))
    (is (= (parse "=" :start :condmatch) ["="]))
    (is (= (parse "!=" :start :condmatch) ["!="]))

    (is (insta/failure? (insta/parse parse ">" :start :condmatch))))

  (testing "condsubquery"
    (is (= (parse "in" :start :condsubquery) ["in"]))

    (is (insta/failure? (insta/parse parse "ni" :start :condsubquery))))

  (testing "valueregexp"
    (is (= (parse "/asdf/" :start :valueregexp) [[:regexp "asdf"]]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "\"asdf\"" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "true" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "/as/df/" :start :valueregexp))))

  (testing "valuenumber"
    (is (= (parse "1" :start :valuenumber) [[:integer "1"]]))
    (is (= (parse "-1" :start :valuenumber) [[:integer "-" "1"]]))
    (is (= (parse "1.1" :start :valuenumber) [[:real "1" "." "1"]]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "\"asdf\"" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "true" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "/asdf/" :start :valuenumber))))

  (testing "valuematch"
    (is (= (parse "'asdf'" :start :valuematch) [[:string "asdf"]]))
    (is (= (parse "1" :start :valuematch) [[:integer "1"]]))
    (is (= (parse "1.1" :start :valuematch) [[:real "1" "." "1"]]))

    (is (insta/failure? (insta/parse parse "asdf" :start :valuematch))))

  (testing "valuesubquery"
    (is (= (parse "(select a from nodes)" :start :valuesubquery)
           [[:from [:select "a"] "nodes"]]))

    (is (insta/failure? (insta/parse parse "select a from nodes" :start :valuesubquery)))))

(deftest test-booleanoperators
  (testing "and"
    (is (= (parse "and" :start :and) []))
    (is (= (parse "  and  " :start :and) []))

    (is (insta/failure? (insta/parse parse "&&" :start :and))))

  (testing "or"
    (is (= (parse "or" :start :or) []))
    (is (= (parse "  or  " :start :or) []))

    (is (insta/failure? (insta/parse parse "||" :start :or))))

  (testing "not"
    (is (= (parse "!" :start :not) [:not]))
    (is (= (parse "  !  " :start :not) [:not]))

    (is (insta/failure? (insta/parse parse "not" :start :not)))))

(deftest test-regexp
  (testing "regexp"
    (is (= (parse "/asdf/" :start :regexp) [:regexp "asdf"]))
    (is (= (parse "/asdf\\/asdf/" :start :regexp) [:regexp "asdf\\/asdf"]))

    (is (insta/failure? (insta/parse parse "not" :start :regexp)))
    (is (insta/failure? (insta/parse parse "/asdf/asdf/" :start :regexp)))
    (is (insta/failure? (insta/parse parse "'asdf'" :start :regexp))))

  (testing "stringwithoutregexpquote"
    (is (= (parse "asdf" :start :stringwithoutregexpquote) ["asdf"]))

    (is (insta/failure? (insta/parse parse "asdf/asdf" :start :stringwithoutregexpquote))))

  (testing "regexpquote"
    (is (= (parse "/" :start :regexpquote) ["/"]))

    (is (insta/failure? (insta/parse parse "'" :start :regexpquote)))))

(deftest test-strings
  (testing "string"
    (is (= (parse "'asdf'" :start :string) [:string "asdf"]))
    (is (= (parse "'as\\'df'" :start :string) [:string "as\\'df"]))
    (is (= (parse "\"asdf\"" :start :string) [:string "asdf"]))
    (is (= (parse "\"as\\\"df\"" :start :string) [:string "as\\\"df"]))

    (is (insta/failure? (insta/parse parse "'asdf\"" :start :string)))
    (is (insta/failure? (insta/parse parse "\"asdf'" :start :string)))
    (is (insta/failure? (insta/parse parse "'asd'asdf'" :start :string))))

  (testing "stringwithoutdoublequotes"
    (is (= (parse "asdf" :start :stringwithoutdoublequotes) ["asdf"]))

    (is (insta/failure? (insta/parse parse "asdf\"asdf" :start :stringwithoutdoublequotes))))

  (testing "stringwithoutsinglequotes"
    (is (= (parse "asdf" :start :stringwithoutsinglequotes) ["asdf"]))

    (is (insta/failure? (insta/parse parse "asdf'asdf" :start :stringwithoutsinglequotes))))

  (testing "singlequote"
    (is (= (parse "'" :start :singlequote) ["'"]))

    (is (insta/failure? (insta/parse parse "`" :start :singlequote))))

  (testing "doublequote"
    (is (= (parse "\"" :start :doublequote) ["\""]))

    (is (insta/failure? (insta/parse parse "'" :start :doublequote)))))

(deftest test-grouping
  (is (= (parse "(" :start :lbracket) ["("]))
  (is (= (parse " (" :start :lbracket) ["("]))
  (is (= (parse "  (  " :start :lbracket) ["("]))
  (is (= (parse ")" :start :rbracket) [")"]))
  (is (= (parse " )" :start :rbracket) [")"]))
  (is (= (parse "  )  " :start :rbracket) [")"]))

  (is (insta/failure? (insta/parse parse "[" :start :lbracket)))
  (is (insta/failure? (insta/parse parse ")" :start :lbracket)))

  (is (insta/failure? (insta/parse parse "]" :start :rbracket)))
  (is (insta/failure? (insta/parse parse "(" :start :rbracket))))

(deftest test-booleans
  (testing "boolean"
    (is (= (parse "true" :start :boolean) [:boolean [:true]]))
    (is (= (parse "false" :start :boolean) [:boolean [:false]]))

    (is (insta/failure? (insta/parse parse "on" :start :boolean)))
    (is (insta/failure? (insta/parse parse "'true'" :start :boolean)))))

(deftest test-numbers
  (testing "integer"
    (is (= (parse "1" :start :integer) [:integer "1"]))
    (is (= (parse "555" :start :integer) [:integer "555"]))
    (is (= (parse "-1" :start :integer) [:integer "-" "1"]))

    (is (insta/failure? (insta/parse parse "- 1" :start :integer))))

  (testing "real"
    (is (= (parse "1.1" :start :real) [:real "1" "." "1"]))
    (is (= (parse "1.1E1" :start :real) [:real "1" "." "1" [:exp "1"]]))
    (is (= (parse "123.123E123" :start :real) [:real "123" "." "123" [:exp "123"]]))
    (is (= (parse "-1.1" :start :real) [:real "-" "1" "." "1"]))

    (is (insta/failure? (insta/parse parse "1." :start :real)))
    (is (insta/failure? (insta/parse parse ".1" :start :real)))
    (is (insta/failure? (insta/parse parse "." :start :real))))

  (testing "exp"
    (is (= (parse "e45" :start :exp) [:exp "45"]))
    (is (= (parse "E45" :start :exp) [:exp "45"]))
    (is (= (parse "E-45" :start :exp) [:exp "-" "45"]))

    (is (insta/failure? (insta/parse parse "E 45" :start :exp))))

  (testing "digits"
    (is (= (parse "1" :start :digits) ["1"]))
    (is (= (parse "123" :start :digits) ["123"]))
    (is (= (parse "555" :start :digits) ["555"]))

    (is (insta/failure? (insta/parse parse "1 1" :start :digits))))

  (testing "negative"
    (is (= (parse "-" :start :negative) ["-"]))

    (is (insta/failure? (insta/parse parse "+" :start :negative)))))

(deftest test-whitespace
  (is (= (parse " " :start :whitespace) [" "]))
  (is (= (parse "  " :start :whitespace) ["  "]))
  (is (= (parse "\t" :start :whitespace) ["\t"]))
  (is (= (parse "\n" :start :whitespace) ["\n"]))
  (is (= (parse "\r\n" :start :whitespace) ["\r\n"]))
  (is (= (parse "  \n" :start :whitespace) ["  \n"]))

  (is (insta/failure? (insta/parse parse "a" :start :whitespace))))
