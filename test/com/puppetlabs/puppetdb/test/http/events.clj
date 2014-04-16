(ns com.puppetlabs.puppetdb.test.http.events
  (:require [com.puppetlabs.puppetdb.reports :as report]
            [puppetlabs.kitchensink.core :as kitchensink]
            [com.puppetlabs.http :as pl-http]
            [com.puppetlabs.puppetdb.scf.storage :as scf-store]
            [cheshire.core :as json]
            [com.puppetlabs.puppetdb.testutils.events :refer [http-expected-resource-events]])
  (:use clojure.test
        [clojure.walk :only [stringify-keys]]
        ring.mock.request
        com.puppetlabs.puppetdb.examples.reports
        com.puppetlabs.puppetdb.fixtures
        [clj-time.core :only [ago now secs]]
        [clj-time.coerce :only [to-string to-long to-timestamp]]
        [com.puppetlabs.puppetdb.testutils :only (response-equal? assert-success! get-request paged-results)]
        [com.puppetlabs.puppetdb.testutils.reports :only (store-example-report! get-events-map)]))

(def v3-endpoint "/v3/events")
(def v4-endpoint "/v4/events")
(def endpoints [[:v3 v3-endpoint]
                [:v4 v4-endpoint]])

(def content-type-json pl-http/json-response-content-type)

(defixture super-fixture :each with-test-db with-http-app)

(defn get-response
  ([endpoint query]
    (get-response endpoint query {}))
  ([endpoint query extra-query-params]
    (*app* (get-request endpoint query extra-query-params))))

(defn munge-event-values
  "Munge the event values that we get back from the web to a format suitable
  for comparison with test data.  This generally involves things like converting
  map keys from keywords to strings, etc."
  [events]
  ;; It is possible for the `old-value` and `new-value` field of an event
  ;; to contain values that are complex data types (arrays, maps).  In
  ;; the case where one of these values is a map, we will get it back
  ;; with keywords as keys, but real world-data has strings as keys.  Here
  ;; we simply convert the keys to strings so that we can compare them for
  ;; tests.
  (map #(kitchensink/maptrans {[:old-value :new-value] stringify-keys} %) events))

(deftest query-by-report
  (doseq [[version endpoint] endpoints]
    (super-fixture
      (fn []
        (let [basic             (store-example-report! (:basic reports) (now))
              basic-events      (get-in reports [:basic :resource-events])
              basic-events-map  (get-events-map (:basic reports))
              report-hash       (:hash basic)]

          ;; TODO: test invalid requests

          (testing "should return the list of resource events for a given report hash"
            (let [response (get-response endpoint ["=" "report" report-hash])
                  expected (http-expected-resource-events version basic-events basic)]
              (response-equal? response expected munge-event-values)))

          (testing "query exceeding event-query-limit"
            (with-http-app {:event-query-limit 1}
              (fn []
                (let [response (get-response endpoint ["=" "report" report-hash])
                      body     (get response :body "null")]
                  (is (= (:status response) pl-http/status-internal-error))
                  (is (re-find #"more than the maximum number of results" body))))))

          ;; NOTE: more exhaustive testing for these queries can be found in
          ;; `com.puppetlabs.puppetdb.test.query.event`
          (testing "should support querying resource events by timestamp"
            (let [start-time  "2011-01-01T12:00:01-03:00"
                  end-time    "2011-01-01T12:00:03-03:00"]

              (testing "should support single term timestamp queries"
                (let [response (get-response endpoint ["<" "timestamp" end-time])
                      expected (http-expected-resource-events
                                version
                                (kitchensink/select-values basic-events-map [1 3])
                                basic)]
                  (response-equal? response expected munge-event-values)))

              (testing "should support compound timestamp queries"
                (let [response (get-response endpoint ["and" [">" "timestamp" start-time]
                                                       ["<" "timestamp" end-time]])
                      expected (http-expected-resource-events
                                version
                                (kitchensink/select-values basic-events-map [3])
                                basic)]
                  (response-equal? response expected munge-event-values)))))

          (testing "compound queries"
            (doseq [[query matches]
                    [[["and"
                       ["or"
                        ["=" "resource-title" "hi"]
                        ["=" "resource-title" "notify, yo"]]
                       ["=" "status" "success"]]                       [1]]
                     [["or"
                       ["and"
                        ["=" "resource-title" "hi"]
                        ["=" "status" "success"]]
                       ["and"
                        ["=" "resource-type" "Notify"]
                        ["=" "property" "message"]]]                  [1 2]]
                     [["and"
                       ["=" "status" "success"]
                       ["<" "timestamp" "2011-01-01T12:00:02-03:00"]]  [1]]
                     [["or"
                       ["=" "status" "skipped"]
                       ["<" "timestamp" "2011-01-01T12:00:02-03:00"]]  [1 3]]]]
              (let [response  (get-response endpoint query)
                    expected  (http-expected-resource-events
                               version
                               (kitchensink/select-values basic-events-map matches)
                               basic)]
                (response-equal? response expected munge-event-values))))


          (doseq [[label count?] [["without" false]
                                  ["with" true]]]
            (testing (str "should support paging through events " label " counts")
              (let [results (paged-results
                             {:app-fn  *app*
                              :path    endpoint
                              :query   ["=" "report" report-hash]
                              :limit   1
                              :total   (count basic-events)
                              :include-total  count?
                              :params  {:order-by (json/generate-string [{"field" "status"}])}})]
                (is (= (count basic-events) (count results)))
                (is (= (http-expected-resource-events
                        version
                        basic-events
                        basic)
                       (set (munge-event-values results)))))))

          (testing "order-by field names"
            (testing "should accept dashes"
              (let [expected  (http-expected-resource-events version basic-events basic)
                    response  (get-response endpoint [">", "timestamp", 0] {:order-by (json/generate-string [{:field "resource-title"}])})]
                (is (= (:status response) pl-http/status-ok))
                (response-equal? response expected munge-event-values)))

            (testing "should reject underscores"
              (let [response  (get-response endpoint [">", "timestamp", 0] {:order-by (json/generate-string [{:field "resource_title"}])})
                    body      (get response :body "null")]
                (is (= (:status response) pl-http/status-bad-request))
                (is (re-find #"Unrecognized column 'resource_title' specified in :order-by" body))))))))))

(deftest query-distinct-resources
  (doseq [[version endpoint] endpoints]
    (super-fixture
      (fn []
        (let [basic             (store-example-report! (:basic reports) (now))
              basic-events      (get-in reports [:basic :resource-events])
              basic-events-map  (get-events-map (:basic reports))

              basic3            (store-example-report! (:basic3 reports) (now))
              basic3-events     (get-in reports [:basic3 :resource-events])
              basic3-events-map (get-events-map (:basic3 reports))]

          (testing "should return an error if the caller passes :distinct-resources without timestamps"
            (let [response  (get-response endpoint ["=" "certname" "foo.local"] {:distinct-resources true})
                  body      (get response :body "null")]
              (is (= (:status response) pl-http/status-bad-request))
              (is (re-find
                    #"'distinct-resources' query parameter requires accompanying parameters 'distinct-start-time' and 'distinct-end-time'"
                    body)))
            (let [response  (get-response endpoint ["=" "certname" "foo.local"] {:distinct-resources true
                                                                                 :distinct-start-time 0})
                  body      (get response :body "null")]
              (is (= (:status response) pl-http/status-bad-request))
              (is (re-find
                    #"'distinct-resources' query parameter requires accompanying parameters 'distinct-start-time' and 'distinct-end-time'"
                    body)))
            (let [response  (get-response endpoint ["=" "certname" "foo.local"] {:distinct-resources true
                                                                                 :distinct-end-time 0})
                  body      (get response :body "null")]
              (is (= (:status response) pl-http/status-bad-request))
              (is (re-find
                    #"'distinct-resources' query parameter requires accompanying parameters 'distinct-start-time' and 'distinct-end-time'"
                    body))))

          (testing "should return only one event for a given resource"
            (let [expected  (http-expected-resource-events version basic3-events basic3)
                  response  (get-response endpoint ["=", "certname", "foo.local"] {:distinct-resources true
                                                                                   :distinct-start-time 0
                                                                                   :distinct-end-time (now)})]
              (assert-success! response)
              (response-equal? response expected munge-event-values)))

          (testing "events should be contained within distinct resource timestamps"
            (let [expected  (http-expected-resource-events version basic-events basic)
                  response  (get-response endpoint ["=", "certname", "foo.local"]
                                                   {:distinct-resources true
                                                    :distinct-start-time 0
                                                    :distinct-end-time "2011-01-02T12:00:01-03:00"})]
              (assert-success! response)
              (response-equal? response expected munge-event-values)))

          (testing "filters (such as status) should be applied *after* the distinct list of most recent events has been built up"
            (let [expected  #{}
                  response (get-response endpoint ["and" ["=" "certname" "foo.local"]
                                                   ["=" "status" "success"]
                                                   ["=" "resource-title" "notify, yar"]]
                                                  {:distinct-resources true
                                                   :distinct-start-time 0
                                                   :distinct-end-time (now)})]
              (assert-success! response)
              (response-equal? response expected munge-event-values))))))))

(deftest query-by-puppet-report-timestamp
  (doseq [[version endpoint] endpoints]
    (super-fixture
      (fn []
        (let [basic         (store-example-report! (:basic reports) (now))
              basic-events  (get-in reports [:basic :resource-events])

              basic3        (store-example-report! (:basic3 reports) (now))
              basic3-events (get-in reports [:basic3 :resource-events])]

          (testing "query by report start time"
            (let [expected  (http-expected-resource-events version basic-events basic)
                  response  (get-response endpoint ["<", "run-start-time" "2011-01-02T00:00:00-03:00"])]
              (assert-success! response)
              (response-equal? response expected munge-event-values)))

          (testing "query by report end time"
            (let [expected  (http-expected-resource-events version basic3-events basic3)
                  response  (get-response endpoint [">", "run-end-time" "2011-01-02T00:00:00-03:00"])]
              (assert-success! response)
              (response-equal? response expected munge-event-values)))

          (testing "query by end time w/no results"
            (let [expected  #{}
                  response  (get-response endpoint [">", "run-end-time" "2011-01-04T00:00:00-03:00"])]
              (assert-success! response)
              (response-equal? response expected munge-event-values))))))))

(deftest query-by-report-receive-timestamp
  (doseq [[version endpoint] endpoints]
    (super-fixture
      (fn []
        (let [test-start-time (ago (secs 1))

              basic           (store-example-report! (:basic reports) (now))
              basic-events    (get-in reports [:basic :resource-events])]
          (testing "query by report receive time"
            (let [expected  (http-expected-resource-events version basic-events basic)
                  response  (get-response endpoint [">", "report-receive-time" (to-string test-start-time)])]
              (assert-success! response)
              (response-equal? response expected munge-event-values)))

          (testing "query by receive time w/no results"
            (let [expected  #{}
                  response  (get-response endpoint ["<", "report-receive-time" (to-string test-start-time)])]
              (assert-success! response)
              (response-equal? response expected munge-event-values))))))))

(deftest valid-subqueries
  (doseq [[version endpoint] endpoints]
    (super-fixture
      (fn []
        (testing (str "subqueries for endpoint: " endpoint ":"))))))
