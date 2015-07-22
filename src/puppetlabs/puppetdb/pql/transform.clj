(ns puppetlabs.puppetdb.pql.transform
  (:require [instaparse.core :as insta]))

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
  {:regexp  transform-regexp
   :string  transform-string
   :boolean transform-boolean
   :integer transform-integer
   :real    transform-real
   :exp     transform-exp})

(defn transform
  [tree]
  (insta/transform transform-specification tree))
