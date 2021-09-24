(ns bluegenes.pages.cetsaadmin.subs
  (:require [re-frame.core :refer [reg-sub]]
            ))

(reg-sub
 :cetsaadmin/root
 (fn [db]
   (:cetsaadmin db)))

(reg-sub
  :cetsaadmin/working?
  :<- [:cetsaadmin/root]
  (fn [root]
    (:working? root)))

(reg-sub
  :cetsaadmin/message
  :<- [:cetsaadmin/root]
  (fn [root]
    (:message root)))

