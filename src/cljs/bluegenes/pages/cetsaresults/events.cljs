(ns bluegenes.pages.cetsaresults.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
            [re-frame.std-interceptors :refer [path]]
            [bluegenes.effects :as fx]
            [imcljs.fetch :as fetch]
            [bluegenes.cetsa :as cx :refer [api-endpoint]]
            [clojure.set :as set]
            [clojure.string :as str]
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
          prot-intersect (apply set/intersection (map #(set (map :uniprot %)) (vals by-drug)))]
      (update-in db root assoc
                 :fetching-res? false
                 :all-results all-res
                 :by-prot by-prot
                 :by-drug by-drug
                 :prot-names prot-info
                 :prot-intersect prot-intersect
                 ))))

(reg-event-db
 :cetsaresults/filter-missing
 (fn [db [_ filter?]]
   (update-in db root assoc :filter-missing filter?)))

(reg-event-db
 :cetsaresults/set-plot-type
 (fn [db [_ p-type]]
   (update-in db root assoc :plot-type p-type)))

(reg-event-fx
 :cetsaresults/failure-im-info
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:cetsaresults :modal :error]
                  (str "Failed to fetch results: " evt))
    :log-error ["Results fetch failure"]}))

(reg-event-fx
 :cetsaresults/success-im-info
 (fn [{db :db} [_ dest lists]]
   (merge
     {:db (assoc-in db dest (:results lists))}
     (when (= :cetsaresults-panel (:active-panel db))
       {:dispatch [:cetsaresults/initialize]}))))

(defn get-q [k v]
  (case k
    :prot {:from "Protein"
           :select ["primaryAccession", "name"]
           :where [{:path "Protein.primaryAccession" :op "ONE OF" :values v}]}
    :drug {:from "DrugCompound"
           :select ["drugBankId", "name"]
           :where [{:path "DrugCompound.drugBankId" :op "ONE OF" :values v}]}))

(reg-event-fx
 :cetsaresults/im-info-from-id
 (fn [{db :db} [_ class id dest]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         q (get-q class id)]
     {:db db
      :im-chan {:chan (fetch/rows service q {:format "jsonobjects"})
                :on-success [:cetsaresults/success-im-info dest]
                :on-failure [:cetsaresults/failure-im-info]
                 }})))

(reg-event-fx
 :cetsaresults/failure-get-files
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:cetsaresults :modal :error]
                  (str "Failed to fetch files: " evt))
    :log-error ["Result files fetch failure"]}))

;  todo: display images?
(reg-event-fx
 :cetsaresults/success-get-files
 (fn [{db :db} [_ lists]]
   (let [res (:data lists)]
     {:db (assoc-in db [:cetsaresults :files] res)}
     )))

(reg-event-fx
 :cetsaresults/get-files
 (fn [{db :db} [_ rid]]
   {:db (update-in db root assoc
                   :files [])
    ::cx/http {:uri (str api-endpoint "?q=list-files&id=" rid)
               :method :get
               :on-success [:cetsaresults/success-get-files]
               :on-error [:cetsaresults/failure-get-files]
               }}))


(reg-event-fx
 :cetsaresults/failure-get-results
 (fn [{db :db} [_ evt]]
   {:db (assoc-in db [:cetsaresults :modal :error]
                  (str "Failed to fetch results: " evt))
    :log-error ["Results fetch failure"]}))

(reg-event-fx
 :cetsaresults/success-get-results
 (fn [{db :db} [_ rid lists]]
   (let [res (:data lists)
         plist (map :uniprot res)]
     (merge
       {:db (assoc-in db [:assets :cetsaresults] res)}
       {:dispatch-n [[:cetsaresults/get-files rid]
                     [:cetsaresults/im-info-from-id :prot plist [:assets :cetsaresults-prot]]]} ))))

(reg-event-fx
 :cetsaresults/get-results
 (fn [{db :db} [_ rid]]
   {:db (update-in db root assoc
                   :fetching-res? true
                   :by-prot {}
                   :by-drug {}
                   :plot-type :volcano
                   )
    ::cx/http {:uri (str api-endpoint "?q=result&id=" rid)
               :method :get
               :on-success [:cetsaresults/success-get-results rid]
               :on-error [:cetsaresults/failure-get-results]
               }}))

(reg-event-fx
 :cetsaresults/failure-get-results-by
 (fn [{db :db} [_ evt]]
   {:log-error ["Results fetch failure"]}))

(reg-event-fx
 :cetsaresults/success-get-results-by
 (fn [{db :db} [_ class lists]]
   (let [ckeys (case class
                 :drug [:uniprot]
                 :prot [:drugname :drugbankID :inchikey])
         res (->> (:data lists)
                  (map #(select-keys % ckeys))
                  distinct)]
     (if  (= class :prot)
       {:dispatch [:cetsaresults/success-im-info [:report :cetsa] {:results res}]}
       {:dispatch [:cetsaresults/im-info-from-id :prot (flatten (map vals res)) [:report :cetsa]]}))))

(reg-event-fx
 :cetsaresults/get-results-by
 (fn [{db :db} [_ class id]]
   {::cx/http {:uri (str api-endpoint "?q=" class "&id=" id)
               :method :get
               :on-success [:cetsaresults/success-get-results-by (keyword (subs class 0 4))]
               :on-error [:cetsaresults/failure-get-results-by]
               }}))

