(ns com.puppetlabs.puppetdb.test.http.metrics
  (:import (java.util.concurrent TimeUnit))
  (:require [com.puppetlabs.http :as pl-http]
            [cheshire.core :as json]
            [clojure.test :refer :all]
            [com.puppetlabs.puppetdb.http.metrics :refer :all]
            [com.puppetlabs.puppetdb.fixtures :as fixt]
            [com.puppetlabs.puppetdb.testutils :refer [get-request deftestseq
                                                       content-type]]))

(use-fixtures :each fixt/with-test-db fixt/with-test-mq fixt/with-http-app)

(def endpoints [[:v2 "/v2/metrics"]
                [:v3 "/v3/metrics"]
                [:v4 "/v4/metrics"]])

(deftest mean-filtering
  (testing "MBean filtering"
    (testing "should pass-through serializable values"
      (is (= (filter-mbean {:key 123})
             {:key 123}))

      (testing "in nested structures"
        (is (= (filter-mbean {:key {:key 123}})
               {:key {:key 123}}))))

    (testing "should stringify unserializable objects"
      (is (= (filter-mbean {:key TimeUnit/SECONDS})
             {:key "SECONDS"}))

      (testing "in nested structures"
        (is (= (filter-mbean {:key {:key TimeUnit/SECONDS}})
               {:key {:key "SECONDS"}}))))))

(defn accepts-plain-text
  "Changes the request to handle text/plain responses"
  [req]
  (assoc-in req [:headers "accept"] "text/plain"))

(deftestseq metrics-set-handler
  [[version endpoint] endpoints]

  (testing "Remote metrics endpoint"
    (testing "should return a pl-http/status-not-found for an unknown metric"
      (let [response (fixt/*app* (get-request (str endpoint "/mbean/does_not_exist")))]
        (is (= (:status response)
               pl-http/status-not-found))))

    (testing "should return a pl-http/status-not-acceptable for unacceptable content type"
      (let [response (fixt/*app* (accepts-plain-text (get-request (str endpoint "/mbeans"))))]
        (is (= (:status response)
               pl-http/status-not-acceptable))))

    (testing "should return a pl-http/status-ok for an existing metric"
      (let [response (fixt/*app* (get-request (str endpoint "/mbean/java.lang:type=Memory")))]
        (is (= (:status response)
               pl-http/status-ok))
        (is (= (content-type response)
               pl-http/json-response-content-type))
        (is (true? (map? (json/parse-string (:body response) true))))))

    (testing "should return a list of all mbeans"
      (let [response (fixt/*app* (get-request (str endpoint "/mbeans")))]
        (is (= (:status response)
               pl-http/status-ok))
        (is (= (content-type response)
               pl-http/json-response-content-type))

        ;; Retrieving all the resulting mbeans should work
        (let [api-mbeans (json/parse-string (:body response))]

          (is (map? api-mbeans))

          (doseq [[_ uri] (take 100 api-mbeans)
                  :let [response (fixt/*app*
                                  (get-request
                                   (str "/" (name version) uri)))]]
            (is (= (:status response)
                   pl-http/status-ok))
            (is (= (content-type response)
                   pl-http/json-response-content-type))))))))
