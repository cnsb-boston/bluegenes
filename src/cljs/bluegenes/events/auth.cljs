(ns bluegenes.events.auth
  (:require [re-frame.core :refer [reg-event-db reg-event-fx]]
            [clojure.string :as str]
            [bluegenes.effects :as fx]
            [bluegenes.route :as route]
            [goog.crypt.base64 :as base64]
            [bluegenes.config :refer [server-vars]]
            [imcljs.auth :as im-auth]
            [bluegenes.interceptors :refer [origin]]))

(defn slim-service
  "Constrains a service map to only the keys needed by the backend API."
  [service]
  (select-keys service [:root :token]))

(defn renamedLists->message [renamedLists]
  [:messages/add
   {:markup [:div
             [:p "The following lists have been renamed due to their name conflicting with an existing list."]
             (into [:ul]
                   (for [[old-kw new-name] renamedLists]
                     [:li (name old-kw) " → " new-name]))]
    :timeout 15000
    :style "info"}])

(defn auth-api-endpoint [op]
  (str (:auth-api @server-vars) "/" op ".php"))

(reg-event-fx
 ::login
 ;; Fire events to log in a user
 (fn [{db :db} [_ username password]]
   (let [current-mine (:local-mine db)
         basic-auth (fn [u p] {"Content-Type" "application/x-www-form-urlencoded"
                               "Authorization" (str "Basic " (base64/encodeString (str u ":" p) false))})]
     {:db (update-in db [:mines current-mine :auth] assoc
                     :username username
                     :thinking? true
                     :error? false)
      ::fx/http {:uri (auth-api-endpoint "login")
                 :method :post
                 :headers (basic-auth username password)
                 :on-success [::login-success]
                 :on-failure [::login-failure]
                 :on-unauthorised [::login-failure]
                 :form-params {:grant_type "password" :username username :password password}}})))

(reg-event-db
 ::login-success
 ;; Store a user's identity and assoc their token to the service of the current mine,
 ;; then (re)fetch the user's lists.
 (fn [db [_ {:keys [access_token refresh_token]}]]
   (let [current-mine (:local-mine db)
         username (get-in db [:mines current-mine :auth :username])
         ]
     (-> db
              (update-in [:mines current-mine :auth] assoc
                         :thinking? false
                         :identity {:preferences {:email username}}
                         :message "ok"
                         :error? false)
              (assoc-in [:mines current-mine :service] {:access access_token :refresh refresh_token})

              )
      )))

(reg-event-db
 ::login-failure
 ;; Clear a user's identity and store an error message
 (fn [db [_ res]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :thinking? false
              :identity nil
              :error? true
              :message (get-in res [:body :error]))))

(reg-event-fx
 ::logout
 ;; Fire events to log out a user. This clears the Session on the server
 (fn [{db :db} [_]]
   (let [current-mine (:local-mine db)]
     {:db (update-in db [:mines current-mine :auth] assoc
                     :thinking? true)
      ::fx/http {:uri (auth-api-endpoint "logout")
                 :method :post
                 :on-success [::logout-success]
                 ;; We don't really care if anything goes wrong.
                 :on-failure [::logout-success]
                 :on-unauthorised [::logout-success]
                 :form-params {:token (get-in db [:mines current-mine :service :access])}}})))

(reg-event-fx
 ::logout-success
 ;; Clear the user's identity and reboot the application
 (fn [{db :db} [_ _response]]
   (let [current-mine (:local-mine db)]
     {:db (-> db
              (update-in [:mines current-mine :auth] assoc
                         :thinking? false
                         :identity nil
                         :error? false
                         :message nil)
              (assoc-in [:mines current-mine :service] nil)
              (route/force-controllers-rerun))
      :dispatch-n [[:remove-login current-mine]
                   [:reboot]]})))

(reg-event-fx
 ::register
 (fn [{db :db} [_ username password]]
   (let [current-mine (:local-mine db)]
     {:db (update-in db [:mines current-mine :auth] assoc
                     :thinking? true
                     :error? false)
      ::fx/http {:uri (auth-api-endpoint "register")
                 :method :post
                 :on-success [::login-success]
                 :on-failure [::login-failure]
                 :on-unauthorised [::login-failure]
                 :form-params {:username username
                               :password password
                     }}})))

(reg-event-fx
 ::request-reset-password
 [(origin)]
 (fn [{db :db origin :origin} [_ email]]
   (let [service (get-in db [:mines (:local-mine db) :service])
         redirectUrl (str origin (route/href ::route/resetpassword))]
     {:db (update-in db [:mines (:local-mine db) :auth] assoc
                     :thinking? true)
      :im-chan {:chan (im-auth/request-password-reset service email redirectUrl)
                :on-success [::request-reset-password-success]
                :on-failure [::request-reset-password-failure]}})))

(reg-event-db
 ::request-reset-password-success
 (fn [db [_]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :thinking? false
              :error? false
              :message nil
              :request-reset-success? true)))

(reg-event-db
 ::request-reset-password-failure
 (fn [db [_ res]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :thinking? false
              :error? true
              :message (if (empty? res)
                         "This feature is not supported in this version of Intermine"
                         (or (get-in res [:body :error])
                             "Failed to send recovery email"))
              :request-reset-success? false)))

(reg-event-fx
 ::reset-password
 (fn [{db :db} [_ new-password token]]
   (let [service (get-in db [:mines (:local-mine db) :service])]
     {:db (assoc-in db [:mines (:local-mine db) :auth :reset-password-in-progress?] true)
      :im-chan {:chan (im-auth/password-reset service new-password token)
                :on-success [::reset-password-success]
                :on-failure [::reset-password-failure]
                :on-unauthorised [::reset-password-failure]}})))

(reg-event-db
 ::reset-password-success
 (fn [db [_]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :reset-password-in-progress? false
              :reset-password-success? true
              :reset-password-error nil)))

(reg-event-db
 ::reset-password-failure
 (fn [db [_ {:keys [status] :as res}]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :reset-password-in-progress? false
              :reset-password-success? false
              :reset-password-error (if (= status 405)
                                      "This feature is not supported in this version of Intermine"
                                      (or (get-in res [:body :error])
                                          "Failed to reset password")))))

(reg-event-db
 ::clear-reset-password-page
 (fn [db]
   (update-in db [:mines (:local-mine db) :auth] dissoc
              :reset-password-in-progress?
              :reset-password-success?
              :reset-password-error)))

(reg-event-db
 ::clear-error
 (fn [db]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :error? false
              :message nil
              :request-reset-success? false)))

(reg-event-fx ;; I think this is only used for google auth
 ::oauth2
 [(origin)]
 (fn [{db :db origin :origin} [_ provider]]
   (let [current-mine (:local-mine db)
         service (get-in db [:mines current-mine :service])
         redirect_uri (str origin "/api/auth/oauth2callback?provider=" provider)] ; ?? origin ??
     {:db (update-in db [:mines current-mine :auth] assoc
                     :error? false)
      ::fx/http {:uri (auth-api-endpoint "oauth2authenticator")
                 :method :post
                 :on-success [::oauth2-success redirect_uri]
                 :on-failure [::oauth2-failure]
                 :on-unauthorised [::oauth2-failure]
                 :transit-params {:service (slim-service service)
                                  :mine-id (name current-mine)
                                  :provider provider
                                  :redirect_uri redirect_uri}}})))

(reg-event-fx
 ::oauth2-success
 (fn [{db :db} [_ redirect_uri link]]
   {:external-redirect (str link "&redirect_uri=" (js/encodeURIComponent redirect_uri))}))

(reg-event-db
 ::oauth2-failure
 (fn [db [_ res]]
   (update-in db [:mines (:local-mine db) :auth] assoc
              :error? true
              :message (get-in res [:body :error]))))
