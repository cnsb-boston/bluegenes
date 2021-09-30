(ns bluegenes.pages.cetsaadmin.events
  (:require [re-frame.core :refer [reg-event-db reg-fx reg-event-fx]]
            [bluegenes.effects :as fx]
            [bluegenes.cetsa :as cx :refer [api-endpoint]]
            [clojure.string :as str]))

(def root [:cetsaadmin])

(reg-event-db
 ::init
 (fn [db [_]]
   (update-in db root assoc :working? false :message nil)))

(reg-event-db
 :cetsaadmin/success-setrole
 (fn [db [_ {:keys [data matched changed]}]]
   (update-in db root assoc :working? false :message (if (= matched 0)
                                                       (str "No user matched: " (second data))
                                                       (str "OK: " (second data) " => " (first data))))))

(reg-event-db
 :cetsaadmin/failure-setrole
 (fn [db [_ evt]]
   (update-in db root assoc :working? false :message (str "Error setting roles: " evt))))

(reg-event-fx
 :cetsaadmin/setrole
 (fn [{db :db} [_ form]]
   (let [args {:username (:username form)
               :scope (case (:role form)
                        :user "user"
                        :member "user privatedata privatemetadata"
                        :admin "user privatedata privatemetadata admin"
                        )
               }]
     {:db (assoc-in db (concat root [:working?]) true)
      ::cx/http {:uri api-endpoint
                 :method :post
                 :on-success [:cetsaadmin/success-setrole]
                 :on-error [:cetsaadmin/failure-setrole]
                 :json-params {:q "set-scope" :data args}
                 }
      :dispatch [:projects/close-modal]
      })))
