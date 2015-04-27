(ns puppetlabs.puppetdb.cthun
  (:require [clojure.tools.logging :as log]
            [puppetlabs.cthun.client :as client]
            [puppetlabs.cthun.message :as message]
            [puppetlabs.kitchensink.core :as kitchensink]))

(defn default-request-handler
  [conn request]
  (log/info "Default handler got message" request))

;; TODO: This should really be grabbing data from the global configuration
(defn get-conn
  []
  (client/connect
   {:server "wss://localhost:8090/cthun/"
    :cert "/Users/ken/Development/cthun/test-resources/ssl/certs/0001_controller.pem"
    :private-key "/Users/ken/Development/cthun/test-resources/ssl/private_keys/0001_controller.pem"
    :cacert "/Users/ken/Development/cthun/test-resources/ssl/certs/ca.pem"
    :identity "cth://0001_controller/puppetdb-service"
    :type "puppetdb-service"}
   ;; We have no specific request requirements today, this is just a placeholder
   {:default default-request-handler}))

(defn send-msg
  [conn id data]
  (client/send! conn
                (-> (message/make-message)
                    (message/set-expiry 60 :seconds)
                    (assoc :id id
                           :endpoints ["cth://*/puppetdb-command"]
                           ;; We'd need to produce our own potentially
                           :data_schema "example/any_schema")
                    (message/set-json-data data))))

;; Ordinarily we'd just be using a persistent connection here, but for now we connect/msg/teardown
(defn send-data
  [id data]
  (let [conn (get-conn)]
    (send-msg conn id data)
    (client/close conn)))
