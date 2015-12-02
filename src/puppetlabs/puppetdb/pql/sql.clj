(ns puppetlabs.puppetdb.pql.sql
  (:require [instaparse.core :as insta]))

(defn transform-from
  ([entity] ["from" entity])
  ;; TODO: right now there are 2 forms of 2-arity functions
  ([arg1 arg2]
   (if (string? arg1)
     ["from" arg1 arg2]
     ["from" arg2 arg1]))
  ([select entity expression]
   ["from" entity (apply vector (concat select [expression]))]))

(defn transform-select
  [& args]
  ["extract" (apply vector args)])

(defn transform-expr4
  ;; Single arg? collapse
  ([data] data)
  ;; Multiple args? turn it into an or statement
  ([data & args] (concat ["or"] [data] args)))

(defn transform-expr3
  ;; Single arg? collapse
  ([data] data)
  ;; Multiple args? turn it into an and statement
  ([data & args] (concat ["and"] [data] args)))

(defn transform-expr2
  ;; Collapse with no args
  ([])
  ;; Single arg? Just collapse the :expr2 and pass back the data,
  ;; closing the nesting.
  ([data] data)
  ;; Two args? This means :not [data] so convert it into a "not"
  ([_ data] ["not" data])
  ;; Three args? only use the third one for a "not"
  ([_ _ data] ["not" data]))

(defn transform-condexpression
  [a b c]
  [b a c])

(defn transform-groupedfieldlist
  [& args]
  (apply vector args))

(defn transform-regexp
  ;; TODO: need to strip escaped backslashes
  [s]
  s)

(defn transform-string
  ;; TODO: need to strip escaped quotes, this will mean we probably
  ;; need to know if the string was doublequoted or singlequoted.
  [s]
  s)

(defn transform-boolean
  [bool]
  (case (first bool)
    :true true
    :false false))

(defn transform-integer
  ([int]
   (Integer. int))
  ([neg int]
   (- (Integer. int))))

(defn transform-real
  [& args]
  (Double. (apply str args)))

(defn transform-exp
  ([int]
   (str "E" int))
  ([neg int]
   (str "E-" int)))

(def transform-specification
  {:from             transform-from
   :select           transform-select
   :expr4            transform-expr4
   :expr3            transform-expr3
   :expr2            transform-expr2
   :condexpression   transform-condexpression
   :groupedfieldlist transform-groupedfieldlist
   :regexp           transform-regexp
   :string           transform-string
   :boolean          transform-boolean
   :integer          transform-integer
   :real             transform-real
   :exp              transform-exp})

(defn transform
  [tree]
  (insta/transform transform-specification tree))

(def parse
  (insta/parser
   (clojure.java.io/resource "puppetlabs/puppetdb/pql/sql.ebnf")))
