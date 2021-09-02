(ns bluegenes.pages.cetsaresults.subs
  (:require [re-frame.core :refer [reg-sub]]
            [clojure.string :as str]))

(reg-sub
 :cetsaresults/root
 (fn [db]
   (:cetsaresults db)))

(reg-sub
 :cetsaresults/all-results
 :<- [:cetsaresults/root]
 (fn [root]
   (:all-results root)))

(reg-sub
 :cetsaresults/by-drug
 :<- [:cetsaresults/root]
 (fn [root]
   (:by-drug root)))

(reg-sub
 :cetsaresults/fetching?
 :<- [:cetsaresults/root]
 (fn [root]
   (:fetching-res? root)) )

(reg-sub
 :cetsaresults/by-prot
 :<- [:cetsaresults/root]
 (fn [root]
   (:by-prot root)))

(reg-sub
 :cetsaresults/prot-name
 :<- [:cetsaresults/prot-names]
 (fn [pnames prot]
   (get-in pnames [prot :prot-name])))

(reg-sub
 :cetsaresults/prot-names
 :<- [:cetsaresults/root]
 (fn [root]
   (:prot-names root)))

(reg-sub
 :cetsaresults/plot-type
 :<- [:cetsaresults/root]
 (fn [root]
   (:plot-type root)))

(reg-sub
 :cetsaresults/no-curves?
 :<- [:cetsaresults/all-results]
 (fn [res]
   (-> res
       first
       :fold_change
       (str/split #",")
       count
       (< 3)
       )))

(reg-sub
 :cetsaresults/drugs
 :<- [:cetsaresults/all-results]
 (fn [res]
   (->> res (map :drugname) distinct sort)))
