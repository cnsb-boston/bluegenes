(ns bluegenes.pages.cetsaresults.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
            [re-frame.std-interceptors :refer [path]]
            [bluegenes.effects :as fx]
            [imcljs.fetch :as fetch]
            [bluegenes.cetsa :refer [api-endpoint]]
            [clojure.set :as set] [clojure.string :as str]
            [bluegenes.db :refer [default-db]]
            [goog.dom :as gdom]
            [oops.core :refer [oset!]]))

(def root [:cetsaresults])

(defn inc-or [a b]
  (if a
    (inc a)
    (inc b)))

(defn reorder-res [res ka kb]
  (loop [r res
         ret {}
         counts {}]
    (if (empty? r)
      ret
      (let [fr (first r)
            rep (get-in counts [(ka fr) (kb fr)] 1)]
        (recur (rest r)
               (update-in ret [(ka fr)] conj (assoc fr :rep rep))
               (update-in counts [(ka fr) (kb fr)] inc-or rep))))))

;{prot-a {plotinfo} ...}
(defn prot-order [res]
  (reorder-res res :uniprot :drugname))

;{drug-a {plotinfo} ...}
(defn drug-order [res]
  (reorder-res res :drugname :uniprot))

(defn select-values [map ks]
  (reduce #(conj %1 (map %2)) [] ks))

(reg-event-db
  :cetsaresults/initialize
  (fn [db]
    (let [prot-info (->> (get-in db [:assets :cetsaresults-prot])
                          (map (fn [m] {(:primaryAccession m) {:prot-id (:objectId m) :prot-name (:name m)}}))
                          (reduce merge))
          all-res (->> (get-in db [:assets :cetsaresults])
                       (map (fn [m] (merge m (get prot-info (:uniprot m))))))
          by-prot (prot-order all-res)
          by-drug (drug-order all-res)
          ]
      (update-in db root assoc
                 :fetching-res? false
                 :all-results all-res
                 :by-prot by-prot
                 :by-drug by-drug
                 :prot-names prot-info
                 ))))

(reg-event-db
 :cetsaresults/set-plot-type
 (fn [db [_ p-type]]
   (update-in db root assoc :plot-type p-type)))

(reg-event-fx
 :cetsaresults/failure-protein-info
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:cetsaresults :modal :error]
                  (str "Failed to fetch results: " evt))
    :log-error ["Results fetch failure"]}))

(reg-event-fx
 :cetsaresults/success-protein-info
 (fn [{db :db} [_ lists]]
   (merge
     {:db (assoc-in db [:assets :cetsaresults-prot] (:results lists))}
     (when (= :cetsaresults-panel (:active-panel db))
       {:dispatch [:cetsaresults/initialize]}))))

(reg-event-fx
 :cetsaresults/protein-info
 (fn [{db :db} [_ uniprot]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         q {:from "Protein"
            :select ["primaryAccession", "name"]
            :where [{:path "Protein.primaryAccession" :op "ONE OF" :values uniprot}] } ]
     {:db db
      :im-chan {:chan (fetch/rows service q {:format "jsonobjects"})
                :on-success [:cetsaresults/success-protein-info]
                :on-failure [:cetsaresults/failure-protein-info]
                 }})))

(reg-event-fx
 :cetsaresults/failure-get-results
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:cetsaresults :modal :error]
                  (str "Failed to fetch results: " evt))
    :log-error ["Results fetch failure"]}))

(reg-event-fx
 :cetsaresults/success-get-results
 (fn [{db :db} [_ lists]]
   (let [res (:data lists)
         plist (map :uniprot res)]
     (merge
       {:db (assoc-in db [:assets :cetsaresults] res)}
       {:dispatch [:cetsaresults/protein-info plist]}))))

(reg-event-fx
 :cetsaresults/get-results
 (fn [{db :db} [_ rid]]
   (let [service (get-in db [:mines (:local-mine db) :service])]
     {
      ;:db (assoc-in db (concat root [:fetching-res?]) true)
      :db (update-in db root assoc
                     :fetching-res? true
                     :by-prot {}
                     :by-drug {}
                     :plot-type :volcano
                     )
      ::fx/http {:uri (str api-endpoint "?q=result&id=" rid)
                 :method :get
                 :headers {"Auth" (str "Bearer " (:access service))}
                 :on-success [:cetsaresults/success-get-results]
                 :on-failure [:cetsaresults/failure-get-results]
                 :on-unauthorised [:cetsaresults/failure-get-results]
                 }})))

