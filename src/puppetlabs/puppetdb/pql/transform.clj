(ns puppetlabs.puppetdb.pql.transform
  (:require [instaparse.core :as insta]))

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
  ;; Single arg? Just collapse the :expr2 and pass back the data,
  ;; closing the nesting.
  ([data] data)
  ;; Two args? This means :not [data] so convert it into a "not"
  ([_ data] ["not" data])
  ;; Fall through
  ;; TODO: the extra arg here is a null, its caused I think by nil
  ;; expr1, if we don't expose expr1 this might go away?
  ([_ _ data] ["not" data]))

(defn transform-expr1
  ;; In certain cases expr1 can have no args, so just collapse
  ([])
  ;; TODO: Groups are more like markers, not even sure we need to expose
  ;; these, since we just collapse them.
  ([arg] arg))

(defn transform-not
  []
  :not)

(defn transform-condexpression
  [a b c]
  [b a c])

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
  {:expr4          transform-expr4
   :expr3          transform-expr3
   :expr2          transform-expr2
   :expr1          transform-expr1
   :not            transform-not
   :condexpression transform-condexpression
   :regexp         transform-regexp
   :string         transform-string
   :boolean        transform-boolean
   :integer        transform-integer
   :real           transform-real
   :exp            transform-exp})

(defn transform
  [tree]
  (insta/transform transform-specification tree))
