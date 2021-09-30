(ns bluegenes.pages.projects.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
            [re-frame.std-interceptors :refer [path]]
            [bluegenes.pages.projects.utils :refer [denormalize-lists filtered-list-ids-set]]
            [bluegenes.effects :as fx]
            [bluegenes.cetsa :as cx :refer [api-endpoint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [bluegenes.db :refer [default-db]]
            [goog.dom :as gdom]
            [goog.string :refer [parseInt]]
            [oops.core :refer [oset!]]))

;; Idea for performance improvement:
;; All lists are re-fetched via `:assets/fetch-lists` whenever something
;; changes, although often fetching a single list would be enough. By using
;; imcljs' `fetch/one-list` you can perhaps make things faster.

(def root [:projects])

;; The vector of lists which the webservice gives us is not very amenable to
;; perform computing on, so we denormalize it into suitable data structures.
(reg-event-db
 :projects/initialize
 (fn [db]
   (let [all-exp (get-in db [:assets :projects])
         all-projects (->> all-exp (map :project_name) distinct)
         old-by-id (get-in db (concat root [:by-id]))
         new-by-id (denormalize-lists all-exp)
         new-lists (if (empty? old-by-id)
                     #{}
                     (-> (set/difference (->> new-by-id vals set)
                                         (->> old-by-id vals set))
                         (set/project [:id])))]
     (update-in db root assoc
                :fetching-exp? false
                :by-id new-by-id
                :all-projects (->> all-projects sort)
                :all-types (->> all-exp (map :experiment_type) distinct sort)
                :all-paths '()
                :new-lists new-lists
                :new-hidden-lists #{}
                                    ))))
;; We have to do a bit of heavy lifting to compute :new-lists and
;; :new-hidden-lists (respectively; red icon to indicate a newly changed list,
;; and alert to indicate newly changed lists not visible under current
;; filters). I have thought about this a lot and explored other approaches, but
;; this looks to be the best way.

(defn set-current-page-1 [lists]
  (assoc-in lists [:pagination :current-page] 1))

(defn clear-new-hidden-lists [lists]
  (update lists :new-hidden-lists empty))

(defn reset-filters [lists]
  (let [default-filters (get-in default-db (concat root [:controls :filters]))]
    (assoc-in lists [:controls :filters] default-filters)))

;; Note: Do not dispatch this from more than one place.
;; The input field which changes this value uses debouncing and internal state,
;; so it won't sync with this value except when first mounting.
(reg-event-db
 :projects/set-keywords-filter
 (path root)
 (fn [lists [_ keywords-string]]
   (-> lists
       (assoc-in [:controls :filters :keywords] keywords-string)
       (set-current-page-1)
       (clear-new-hidden-lists))))

(reg-event-db
 :projects/toggle-sort
 (path root)
 (fn [lists [_ column]]
   (update-in lists [:controls :sort]
              (fn [{old-column :column old-order :order}]
                {:column column
                 :order (if (= old-column column)
                          (case old-order
                            :asc :desc
                            :desc :asc)
                          :asc)}))))

(reg-event-db
 :projects/set-filter
 (path root)
 (fn [lists [_ filter-name value]]
   (-> lists
       (assoc-in [:controls :filters filter-name] value)
       (set-current-page-1)
       (clear-new-hidden-lists))))

;; We don't want to make the keyword filter a controlled input as we want to be
;; able to debounce its event. Leading to this lesser evil of DOM manipulation.
(reg-fx
 ::clear-keyword-filter
 (fn [_]
   (oset! (gdom/getElement "lists-keyword-filter") :value "")))

(reg-event-fx
 :projects/reset-filters
 (fn [{db :db} [_]]
   {:db (update-in db root reset-filters)
    ::clear-keyword-filter {}}))

(reg-event-fx
 :projects/show-new-lists
 (fn [{db :db} [_]]
   {:db (-> db
            (update-in root reset-filters)
            (update-in root clear-new-hidden-lists))
    ::clear-keyword-filter {}}))

(reg-event-db
 :projects/set-per-page
 (path root)
 (fn [lists [_ new-value]]
   (assoc-in lists [:pagination :per-page] new-value)))

(reg-event-fx
 :projects/set-current-page
 (fn [{db :db} [_ new-value scroll-top?]]
   (cond-> {:db (assoc-in db (concat root [:pagination :current-page]) new-value)}
     scroll-top? (assoc :scroll-to-top {:ms 0}))))

(reg-event-db
 :projects/select-list
 (path root)
 (fn [lists [_ list-id]]
   (update lists :selected-lists (fnil conj #{}) list-id)))

;; It would be more efficient to use a special value like `:all`, but this would
;; have to be handled in all event handlers reading `:selected-lists`. Not worth
;; it when this is a feature that should be rarely used.
(reg-event-db
 :projects/select-all-lists
 (path root)
 (fn [lists]
   (assoc lists :selected-lists
          (filtered-list-ids-set
           (:by-id lists)
           (get-in lists [:controls :filters])))))

(reg-event-db
 :projects/deselect-exp
 (path root)
 (fn [lists [_ exp-id]]
   (update lists :selected-exp (fnil disj #{}) exp-id)))

(reg-event-db
 :projects/clear-selected
 (path root)
 (fn [lists [_]]
   (assoc lists :selected-lists #{})))

(reg-event-db
 :projects/clear-target
 (path root)
 (fn [lists [_]]
   (let [target-id (get-in lists [:modal :target-id])]
     (update lists :selected-lists (fnil disj #{}) target-id))))

(reg-event-fx
 :projects/open-modal
 (path root)
 (fn [{exp :db} [_ modal-kw ?exp-id]]
   {:db (case modal-kw
          ;; Edit modal needs the list fields preset.
          :edit (let [{:keys [title]} (get-in exp [:by-id] ?exp-id)]
                  (assoc exp :modal
                         {:active modal-kw
                          :target-id ?exp-id
                          :open? true
                          :title title
                          }))
          ;; Default for all other modals.
          (assoc exp :modal
                 {:active modal-kw
                  :target-id ?exp-id
                  :open? true}
                 ))}))

(reg-event-fx
 :projects/failure-modal-edit
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:projects :modal :error]
                  (str "Failed to edit experiments:" evt))
    :log-error ["Experiment edit failure"]}))

(reg-event-fx
 :projects/success-modal-edit
 (fn [{db :db} [_ lists]]
   (merge
    {:db (assoc-in db [:assets :projects] (:data lists))}
    ;; Denormalize lists right-away if you're on the lists page.
    (when (= :projects-panel (:active-panel db))
      {:dispatch [:projects/get-experiments]}))))


(reg-event-fx
 :projects/modal-edit
 (fn [{db :db} [_ modal-form]]
   (let [modal-form (assoc modal-form
                           :private (if (:private modal-form) 1 0)
                           :num_samples (parseInt (:num_samples modal-form))
                           :num_replicates (parseInt (:num_replicates modal-form))
                           :ID (:id modal-form)
                           )]
     {:db (assoc-in db (concat root [:fetching-exp?]) true)
      ::cx/http {:uri api-endpoint
                 :method :post
                 :on-success [:projects/success-modal-edit]
                 :on-error [:projects/failure-modal-edit]
                 :json-params {:q "edit-experiment" :data modal-form}
                 }
      :dispatch [:projects/close-modal]
      })))

(reg-event-db
 :projects/close-modal
 (path root)
 (fn [lists [_]]
   (assoc-in lists [:modal :open?] false)))

(reg-event-db
 :projects-modal/set-new-list-tags
 (path root)
 (fn [lists [_ tags]]
   (assoc-in lists [:modal :tags] tags)))

(reg-event-db
 :projects-modal/set-new-list-title
 (path root)
 (fn [lists [_ title]]
   (assoc-in lists [:modal :title] title)))

(reg-event-db
 :projects-modal/set-new-list-description
 (path root)
 (fn [lists [_ description]]
   (assoc-in lists [:modal :description] description)))

(reg-event-fx
 :projects/failure-get-exp
 (fn [{db :db} [_ evt]]
   (let [db (assoc-in db [:projects :modal :error]
                  (str "Failed to fetch experiments:" evt))
         db (assoc-in db (concat root [:fetching-exp?]) false)]
     {:db db
    :log-error ["Experiments fetch failure"]})))

(reg-event-fx
 :projects/success-get-exp
 (fn [{db :db} [_ lists]]
   (merge
    {:db (assoc-in db [:assets :projects] (:data lists))}
    ;; Denormalize lists right-away if you're on the lists page.
    (when (= :projects-panel (:active-panel db))
      {:dispatch [:projects/initialize]}))))

(reg-event-fx
 :projects/get-experiments
 (fn [{db :db} [_]]
   {:db (assoc-in db (concat root [:fetching-exp?]) true)
    ::cx/http {:uri (str api-endpoint "?q=experiment")
               :method :get
               :on-success [:projects/success-get-exp]
               :on-error [:projects/failure-get-exp]
               }}))

