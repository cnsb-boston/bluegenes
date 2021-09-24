(ns bluegenes.pages.cetsaadmin.views
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]
            [bluegenes.pages.cetsaadmin.events :as events]
            [bluegenes.pages.cetsaadmin.subs :as subs]
            [bluegenes.components.loader :refer [mini-loader]]
            [bluegenes.components.icons :refer [icon]]
            [oops.core :refer [oget ocall]]
            [bluegenes.components.bootstrap :refer [poppable]]
            [bluegenes.utils :refer [md-element]]))

(defn update-form [atom key evt]
  (swap! atom assoc key (oget evt :target :value)))

(defn set-permissions []
  (let [form {:user {:name "role"}
              :member {:name "role"}
              :admin {:name "role"}}
        ks (keys form)
        rform (r/atom {:username "" :role :user})]
    (fn []
      (let [working? @(subscribe [:cetsaadmin/working?])
            message @(subscribe [:cetsaadmin/message])]
        [:div
         [:div.form-outline
          [:input.form-control {:type "text" :name :username 
                                :value (:username @rform)
                                :on-change (partial update-form rform :username)
                                :id "user"
                                :placeholder "Username"
                                }]]
         (for [k ks]
           ^{:key k}
           [:div.form-check
            [:input.form-check-input {:type "radio" :name k
                                      :checked (= k (:role @rform))
                                      :value k
                                      :on-click #(swap! rform assoc :role k)
                                      :readOnly true
                                      }]
            [:label.form-check-label (name k)]])
         [:div
          [:button {:on-click #(dispatch [:cetsaadmin/setrole @rform])} "Submit" ]
          [:button {:on-click #(swap! rform assoc :username "")} "Reset" ]
          [:span (if working?
                   [mini-loader "tiny"]
                   (if message message ""))]]]))
    )
  )

(defn main []
  [:div.admin-page.container
   [:h3 "Set user permissions:"]
   [:div [set-permissions]]
   ])
