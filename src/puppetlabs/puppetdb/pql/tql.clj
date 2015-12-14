(ns puppetlabs.puppetdb.pql.tql
  (:require [instaparse.core :as insta]))

(defn transform-from
  ([entity]
   ["from" entity])
  ([entity arg2]
   ["from" entity arg2])
  ;; TODO: so an extract is a wrapper around a query in AST, but not in PQL, we should consider how to marry this
  ([entity expression select]
   ["from" entity (apply vector (concat select [expression]))]))

;; TODO: this looks a lot like a 'from' but is extraction just 'dumb' for implicit subqueries?
(defn transform-subquery
  ([entity]
   ["subquery" entity])
  ([entity arg2]
   ["subquery" entity arg2])
  ;; TODO: this one, seems dumb, an implicit subquery would throw away these results
  ([entity expression select]
   ["subquery" entity (apply vector (concat select [expression]))]))

(defn transform-select
  [& args]
  ["extract" (apply vector args)])

(defn transform-expr4
  ;; Single arg? collapse
  ([data] data)
  ;; Multiple args? turn it into an or statement
  ([data & args] (vec (concat ["or"] [data] args))))

(defn transform-expr3
  ;; Single arg? collapse
  ([data] data)
  ;; Multiple args? turn it into an and statement
  ([data & args] (vec (concat ["and"] [data] args))))

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
   :subquery         transform-subquery
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
   (clojure.java.io/resource "puppetlabs/puppetdb/pql/tql.ebnf")))
