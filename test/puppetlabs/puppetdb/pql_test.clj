(ns puppetlabs.puppetdb.pql-test
  (:require [clojure.test :refer :all]
            [puppetlabs.puppetdb.pql :refer :all]
            [instaparse.core :as insta]))

;; These tests are ordered the same as in the EBNF file, so one can
;; develop the expressions and tests side-by-side.

(deftest test-expressions
  (is (= (parse "a == 1" :start :expression)
         [:expression]))
  (is (= (parse "a == 1 and b == 2" :start :expression)
         [:expression]))
  (is (= (parse "c == 3 or d == 4 and a == 1" :start :expression)
         [:expression]))
  (is (= (parse "c == 3 or d == 4 and a == 1 or b == 2" :start :expression)
         [:expression]))
  (is (= (parse "(c == 3 or d == 4) and (a == 1 or b == 2)" :start :expression)
         [:expression]))

  (is (insta/failure? (insta/parse parse "foo and 'bar'" :start :expression))))

(deftest test-condexpression
  (testing "condexpression"
    (is (= (parse "certname == 'foobar'" :start :condexpression)
           [:condexpression
            [:field "certname"]
            [:condmatch "=="]
            [:valuematch [:string "foobar"]]]))
    (is (= (parse "certname =~ /foobar/" :start :condexpression)
           [:condexpression
            [:field "certname"]
            [:condregexp "=~"]
            [:valueregexp [:regexp "foobar"]]]))
    (is (= (parse "certname > 4" :start :condexpression)
           [:condexpression
            [:field "certname"]
            [:condnumber ">"]
            [:valuenumber [:integer "4"]]]))

    (is (insta/failure? (insta/parse parse "foo = 'bar'" :start :condexpression)))
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
    (is (= (parse "=~ /asdf/" :start :condexpregexp)
           [[:condregexp "=~"] [:valueregexp [:regexp "asdf"]]]))
    (is (= (parse "!~ /asdf/" :start :condexpregexp)
           [[:condregexp "!~"] [:valueregexp [:regexp "asdf"]]]))

    (is (insta/failure? (insta/parse parse "=~ 'bar'" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "=~ 4" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "=~ true" :start :condexpregexp)))
    (is (insta/failure? (insta/parse parse "!~ 'bar'" :start :condexpregexp))))

  (testing "condexpnumber"
    (is (= (parse ">= 4" :start :condexpnumber)
           [[:condnumber ">="] [:valuenumber [:integer "4"]]]))

    (is (insta/failure? (insta/parse parse ">= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse ">= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "<= 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "<= true" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "> 'bar'" :start :condexpnumber)))
    (is (insta/failure? (insta/parse parse "< true" :start :condexpnumber))))

  (testing "condexpmatch"
    (is (= (parse "== 'bar'" :start :condexpmatch)
           [[:condmatch "=="] [:valuematch [:string "bar"]]]))

    (is (insta/failure? (insta/parse parse "== bar" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "== /bar/" :start :condexpmatch)))
    (is (insta/failure? (insta/parse parse "!= bar" :start :condexpmatch)))))

(deftest test-conditionalexpressionparts
  (testing "field"
    (is (= (parse "certname" :start :field)
           [:field "certname"]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :field))))

  (testing "condregexp"
    (is (= (parse "=~" :start :condregexp)
           [:condregexp "=~"]))
    (is (= (parse "!~" :start :condregexp)
           [:condregexp "!~"]))

    (is (insta/failure? (insta/parse parse "=" :start :condregexp)))
    (is (insta/failure? (insta/parse parse "==" :start :condregexp))))

  (testing "condnumber"
    (is (= (parse ">=" :start :condnumber)
           [:condnumber ">="]))
    (is (= (parse "<=" :start :condnumber)
           [:condnumber "<="]))

    (is (insta/failure? (insta/parse parse "=" :start :condnumber)))
    (is (insta/failure? (insta/parse parse "==" :start :condnumber))))

  (testing "condmatch"
    (is (= (parse "==" :start :condmatch)
           [:condmatch "=="]))
    (is (= (parse "!=" :start :condmatch)
           [:condmatch "!="]))

    (is (insta/failure? (insta/parse parse "=" :start :condmatch)))
    (is (insta/failure? (insta/parse parse ">" :start :condmatch))))

  (testing "valueregexp"
    (is (= (parse "/asdf/" :start :valueregexp)
           [:valueregexp [:regexp "asdf"]]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "\"asdf\"" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "true" :start :valueregexp)))
    (is (insta/failure? (insta/parse parse "/as/df/" :start :valueregexp))))

  (testing "valuenumber"
    (is (= (parse "1" :start :valuenumber)
           [:valuenumber [:integer "1"]]))
    (is (= (parse "-1" :start :valuenumber)
           [:valuenumber [:integer "-" "1"]]))
    (is (= (parse "1.1" :start :valuenumber)
           [:valuenumber [:real "1" "." "1"]]))

    (is (insta/failure? (insta/parse parse "'asdf'" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "\"asdf\"" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "true" :start :valuenumber)))
    (is (insta/failure? (insta/parse parse "/asdf/" :start :valuenumber))))

  (testing "valuematch"
    (is (= (parse "'asdf'" :start :valuematch)
           [:valuematch [:string "asdf"]]))
    (is (= (parse "1" :start :valuematch)
           [:valuematch [:integer "1"]]))
    (is (= (parse "1.1" :start :valuematch)
           [:valuematch [:real "1" "." "1"]]))

    (is (insta/failure? (insta/parse parse "asdf" :start :valuematch)))))

(deftest test-booleanoperators
  (testing "and"
    (is (= (parse "and" :start :and)
           [:and]))
    (is (= (parse "  and  " :start :and)
           [:and]))

    (is (insta/failure? (insta/parse parse "&&" :start :and))))

  (testing "or"
    (is (= (parse "or" :start :or)
           [:or]))
    (is (= (parse "  or  " :start :or)
           [:or]))

    (is (insta/failure? (insta/parse parse "||" :start :or))))

  (testing "not"
    (is (= (parse "!" :start :not)
           [:not]))
    (is (= (parse "  !  " :start :not)
           [:not]))

    (is (insta/failure? (insta/parse parse "not" :start :not)))))

(deftest test-regexp
  (testing "regexp"
    (is (= (parse "/asdf/" :start :regexp)
           [:regexp "asdf"]))
    (is (= (parse "/asdf\\/asdf/" :start :regexp)
           [:regexp "asdf\\/asdf"]))

    (is (insta/failure? (insta/parse parse "not" :start :regexp)))
    (is (insta/failure? (insta/parse parse "/asdf/asdf/" :start :regexp)))
    (is (insta/failure? (insta/parse parse "'asdf'" :start :regexp))))

  (testing "regexpquote"
    (is (= (parse "/" :start :regexpquote)
           ["/"]))

    (is (insta/failure? (insta/parse parse "'" :start :regexpquote)))))

(deftest test-strings
  (testing "string"
    (is (= (parse "'asdf'" :start :string)
           [:string "asdf"]))
    (is (= (parse "'as\\'df'" :start :string)
           [:string "as\\'df"]))
    (is (= (parse "\"asdf\"" :start :string)
           [:string "asdf"]))
    (is (= (parse "\"as\"df\"" :start :string)
           [:string "as\"df"]))

    (is (insta/failure? (insta/parse parse "'asdf\"" :start :string)))
    (is (insta/failure? (insta/parse parse "\"asdf'" :start :string)))
    (is (insta/failure? (insta/parse parse "'asd'asdf'" :start :string))))

  (testing "stringcomponent"
    (is (= (parse "asdf" :start :stringcomponent)
           ["asdf"]))

    (is (insta/failure? (insta/parse parse "asdf\"asdf" :start :stringcomponent)))))

(deftest test-quotes
  (testing "singlequote"
    (is (= (parse "'" :start :singlequote)
           ["'"]))

    (is (insta/failure? (insta/parse parse "`" :start :singlequote))))

  (testing "doublequote"
    (is (= (parse "\"" :start :doublequote)
           ["\""]))

    (is (insta/failure? (insta/parse parse "'" :start :doublequote)))))

(deftest test-grouping
  (is (= (parse "(" :start :lbracket)
         ["("]))
  (is (= (parse " (" :start :lbracket)
         ["("]))
  (is (= (parse "  (  " :start :lbracket)
         ["("]))
  (is (= (parse ")" :start :rbracket)
         [")"]))
  (is (= (parse " )" :start :rbracket)
         [")"]))
  (is (= (parse "  )  " :start :rbracket)
         [")"]))

  (is (insta/failure? (insta/parse parse "[" :start :lbracket)))
  (is (insta/failure? (insta/parse parse ")" :start :lbracket)))

  (is (insta/failure? (insta/parse parse "]" :start :rbracket)))
  (is (insta/failure? (insta/parse parse "(" :start :rbracket))))

(deftest test-booleans
  (testing "boolean"
    (is (= (parse "true" :start :boolean)
           [:boolean [:true]]))
    (is (= (parse "false" :start :boolean)
           [:boolean [:false]]))

    (is (insta/failure? (insta/parse parse "on" :start :boolean)))
    (is (insta/failure? (insta/parse parse "'true'" :start :boolean)))))

(deftest test-numbers
  (testing "integer"
    (is (= (parse "1" :start :integer)
           [:integer "1"]))
    (is (= (parse "555" :start :integer)
           [:integer "555"]))
    (is (= (parse "-1" :start :integer)
           [:integer "-" "1"]))

    (is (insta/failure? (insta/parse parse "- 1" :start :integer))))

  (testing "real"
    (is (= (parse "1.1" :start :real)
           [:real "1" "." "1"]))
    (is (= (parse "1.1E1" :start :real)
           [:real "1" "." "1" [:exp "1"]]))
    (is (= (parse "123.123E123" :start :real)
           [:real "123" "." "123" [:exp "123"]]))

    (is (insta/failure? (insta/parse parse "1." :start :real)))
    (is (insta/failure? (insta/parse parse ".1" :start :real)))
    (is (insta/failure? (insta/parse parse "." :start :real))))

  (testing "exp"
    (is (= (parse "e45" :start :exp)
           [:exp "45"]))
    (is (= (parse "E45" :start :exp)
           [:exp "45"]))
    (is (= (parse "E-45" :start :exp)
           [:exp "-" "45"]))

    (is (insta/failure? (insta/parse parse "E 45" :start :exp))))

  (testing "digits"
    (is (= (parse "1" :start :digits)
           ["1"]))
    (is (= (parse "123" :start :digits)
           ["123"]))
    (is (= (parse "555" :start :digits)
           ["555"]))

    (is (insta/failure? (insta/parse parse "1 1" :start :digits))))

  (testing "negative"
    (is (= (parse "-" :start :negative)
           ["-"]))

    (is (insta/failure? (insta/parse parse "+" :start :negative)))))

(deftest test-whitespace
  (is (= (parse " " :start :whitespace)
         [" "]))
  (is (= (parse "  " :start :whitespace)
         ["  "]))
  (is (= (parse "\t" :start :whitespace)
         ["\t"]))
  (is (= (parse "\n" :start :whitespace)
         ["\n"]))
  (is (= (parse "\r\n" :start :whitespace)
         ["\r\n"]))
  (is (= (parse "  \n" :start :whitespace)
         ["  \n"]))

  (is (insta/failure? (insta/parse parse "a" :start :whitespace))))
