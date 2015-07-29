(ns puppetlabs.puppetdb.pql.parser-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql.parser :refer :all]
            [instaparse.core :as insta]))

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
             [:condexpression "a" "==" [:integer "1"]]]]]])))

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
    (is (= (parse "=~ /asdf/" :start :condexpregexp) ["=~" [:regexp "asdf"]]))
    ;; TODO: there is no concept of !~ in our language today
    #_(is (= (parse "!~ /asdf/" :start :condexpregexp) ["!~" [:regexp "asdf"]]))

    (is (insta/failure? (insta/parse parse "=~ 'bar'" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "=~ 4" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "=~ true" :start :condexpregexp)))
    #_(is (insta/failure? (insta/parse parse "!~ 'bar'" :start :condexpregexp))))

  (testing "condexpnumber"
    (is (= (parse ">= 4" :start :condexpnumber) [">=" [:integer "4"]]))

    (is (insta/failure? (insta/parse parse ">= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse ">= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "<= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "<= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "> 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "< true" :start :condexpnumber))))

  (testing "condexpmatch"
    (is (= (parse "== 'bar'" :start :condexpmatch) ["==" [:string "bar"]]))

    (is (insta/failure? (insta/parse parse "== bar" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "== /bar/" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "!= bar" :start :condexpmatch)))))

(deftest test-conditionalexpressionparts
  (testing "field"
    (is (= (parse "certname" :start :field) ["certname"]))

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

    (is (insta/failure? (insta/parse parse "asdf" :start :valuematch)))))

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
