(ns bluegenes.pages.projects.subs
  (:require [re-frame.core :refer [reg-sub]]
            [bluegenes.pages.projects.utils :refer [normalize-lists ->filterf ->sortf filtered-list-ids-set]]
            [clojure.set :as set]))

(reg-sub
 :projects/root
 (fn [db]
   (:projects db)))

(reg-sub
 :projects/by-id
 :<- [:projects/root]
 (fn [root]
   (:by-id root)))

(reg-sub
 :projects/can-edit-exp?
 (fn [db]
   (get-in db [:mines (:local-mine db) :service :scope :privatemetadata])))

(reg-sub
 :projects/fetching?
 :<- [:projects/root]
 (fn [root]
   (:fetching-exp? root)))

(reg-sub
 :projects/all-projects
 :<- [:projects/root]
 (fn [root]
   (:all-projects root)))

(reg-sub
 :projects/all-types
 :<- [:projects/root]
 (fn [root]
   (:all-types root)))

(reg-sub
 :projects/all-paths
 :<- [:projects/root]
 (fn [root]
   (:all-paths root)))

(reg-sub
 :projects/new-lists
 :<- [:projects/root]
 (fn [root]
   (:new-lists root)))

(reg-sub
 :projects/selected-exp
 :<- [:projects/root]
 (fn [root]
   (:selected-exp root)))

(reg-sub
 :projects/all-selected?
 :<- [:projects/filtered-list-ids-set]
 :<- [:projects/selected-exp]
 (fn [[list-ids selected-exp]]
   (if (empty? selected-exp)
     false
     ;; We use superset instead of equality as there may be more lists selected
     ;; under a different filter.
     (set/superset? selected-exp list-ids))))

(reg-sub
 :projects/pagination
 :<- [:projects/root]
 (fn [root]
   (:pagination root)))

(reg-sub
 :projects/per-page
 :<- [:projects/pagination]
 (fn [pagination]
   (:per-page pagination)))

(reg-sub
 :projects/current-page
 :<- [:projects/pagination]
 (fn [pagination]
   (:current-page pagination)))

(reg-sub
 :projects/controls
 :<- [:projects/root]
 (fn [root]
   (:controls root)))

(reg-sub
 :projects/filters
 :<- [:projects/controls]
 (fn [controls]
   (:filters controls)))

(reg-sub
 :projects/filter
 :<- [:projects/filters]
 (fn [filters [_ filter-name]]
   (get filters filter-name)))

(reg-sub
 :projects/keywords-filter
 :<- [:projects/filters]
 (fn [filters]
   (:keywords filters)))

(reg-sub
 :projects/sort
 :<- [:projects/controls]
 (fn [controls]
   (:sort controls)))

(reg-sub
 :projects/filtered-exp
 :<- [:projects/by-id]
 :<- [:projects/filters]
 :<- [:projects/sort]
 :<- [:projects/pagination]
 (fn [[items-by-id active-filters active-sort pagination]]
   ;(vals items-by-id)
   (normalize-lists
    (->filterf active-filters)
    (->sortf active-sort :folders-first? (= :folder (:projects active-filters)))
    {:by-id items-by-id}
    pagination)
   ))

(reg-sub
 :projects/no-filtered-exp?
 :<- [:projects/filtered-exp]
 (fn [lists]
   (empty? lists)))

;; Although normalize-lists is used here, similar to :projects/filtered-lists,
;; it differs in that all folders are expanded, the items are filtered but
;; not sorted, and we return a set of IDs. This will be the set of all list
;; IDs that are selectable with the currently active filters.
(reg-sub
 :projects/filtered-list-ids-set
 :<- [:projects/by-id]
 :<- [:projects/filters]
 (fn [[items-by-id active-filters]]
   (filtered-list-ids-set items-by-id active-filters)))

(reg-sub
 :projects/no-exp?
 :<- [:projects/by-id]
 (fn [items-by-id]
   (empty? items-by-id)))

(reg-sub
 :projects/top-level-lists
 :<- [:projects/by-id]
 :<- [:projects/filters]
 (fn [[items-by-id active-filters]]
   (normalize-lists
    (->filterf active-filters)
    identity
    {:by-id items-by-id})))

(reg-sub
 :projects/new-hidden-lists
 :<- [:projects/root]
 (fn [root]
   (:new-hidden-lists root)))

(reg-sub
 :projects/top-level-count
 :<- [:projects/top-level-lists]
 (fn [top-level-lists]
   (count top-level-lists)))

(reg-sub
 :projects/page-count
 :<- [:projects/per-page]
 :<- [:projects/top-level-count]
 (fn [[per-page total-count]]
   (if (pos? total-count)
     (Math/ceil (/ total-count per-page))
     0)))

(reg-sub
 :projects/modal-root
 :<- [:projects/root]
 (fn [root]
   (:modal root)))

(reg-sub
 :projects/active-modal
 :<- [:projects/modal-root]
 (fn [modal]
   (:active modal)))

(reg-sub
 :projects/modal-open?
 :<- [:projects/modal-root]
 (fn [modal]
   (:open? modal)))

(reg-sub
 :projects/selected-exp-details
 :<- [:projects/by-id]
 :<- [:projects/selected-exp]
 (fn [[by-id selected-exp]]
   (map by-id selected-exp)))

(reg-sub
 :projects/selected-exp-different-types?
 :<- [:projects/selected-exp-details]
 (fn [lists]
   (->> (map :type lists)
        (apply not=))))

(reg-sub
 :projects/modal-target-exp
 :<- [:projects/modal-root]
 :<- [:projects/by-id]
 (fn [[modal by-id]]
   (get by-id (:target-id modal))))

(reg-sub
 :projects-modal/new-list-tags
 :<- [:projects/modal-root]
 (fn [modal]
   (:tags modal)))

(reg-sub
 :projects-modal/new-list-title
 :<- [:projects/modal-root]
 (fn [modal]
   (:title modal)))

(reg-sub
 :projects-modal/new-list-description
 :<- [:projects/modal-root]
 (fn [modal]
   (:description modal)))

(reg-sub
 :projects-modal/error
 :<- [:projects/modal-root]
 (fn [modal]
   (:error modal)))

(reg-sub
 :projects-modal/target-id
 :<- [:projects/modal-root]
 (fn [modal]
   (:target-id modal)))

(reg-sub
 :projects-modal/target-list
 :<- [:projects/by-id]
 :<- [:projects-modal/target-id]
 (fn [[by-id target-id]]
   (get by-id target-id)))
