(ns bluegenes.cetsa
  (:require [bluegenes.config :refer [server-vars]]
            [re-frame.core :refer [reg-fx reg-event-db reg-event-fx]]
            [bluegenes.effects :as fx]
            [re-frame.db :as rdb]
            [clojure.string :as str]))

(def api-endpoint (str/replace (:auth-api @server-vars) #"auth$" "api.php"))

(reg-event-fx
 ;failure conditions: expired, revoked by server
 ::token-failure
 (fn [{db :db} [_ on-error]]
   {:db (update-in db [:mines (:local-mine db)] dissoc :service :auth)
    :log-error "Credentials expired. Please login again."
    :dispatch on-error
    }))

(reg-fx
 ::http
 (fn [payload]
   (let [db rdb/app-db
         service (get-in @db [:mines (:local-mine @db) :service])
         payload (assoc payload
                        :on-unauthorised [::token-failure (:on-error payload)]
                        :headers (if service {"Auth" (str "Bearer " (:access service))} {}))]
     (fx/http-fxfn payload))))
