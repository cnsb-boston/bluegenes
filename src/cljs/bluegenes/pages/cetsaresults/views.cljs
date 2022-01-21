(ns bluegenes.pages.cetsaresults.views
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]
            [bluegenes.components.icons :refer [icon icon-comp]]
            [bluegenes.components.loader :refer [mini-loader]]
            [oops.core :refer [oget oset! ocall]]
            [goog.functions :refer [debounce]]
            [bluegenes.components.select-tags :as select-tags]
            [bluegenes.components.viz.common :refer [vega-lite]]
            [bluegenes.subs.auth :as auth]
            [clojure.string :as str]
            [bluegenes.route :as route]
            [bluegenes.components.bootstrap :refer [poppable]]
            [goog.string :as gstring]))

(defn drugs []
  [:div
   [:h4 "Drug reports:"]
   [:div.grid-4
   (for [d @(subscribe [:cetsaresults/drugs])]
       ^{:key (keyword d)}
     [:div.col
      {:style {:text-decoration "underline" :cursor "pointer" :border-style "solid" :border-width 1 :text-align "center"}
       :on-click #(do
                    (dispatch [:search/full-search d])
                    (dispatch [::route/navigate ::route/search nil {:keyword d}]))} ;;; TODO: by pubdbID?
      d]
     )
   ]]
  )

(defn vegaspec-heat [data]
  {:description "Heatmap",
   :title "Protein-Drug abundance"
   :width 600
   :height 600
   :config {:view {:strokeWidth 0 :step 13}
            :axis {:domain false}
            :range {:heatmap {:scheme "viridis"}}
            }
   :data {:values data}
   :mark "rect"
   :encoding {:x {:type "ordinal"
                  :field "drugbankID"
                  }
              :y {:field "uniprot"
                  :type "ordinal"}
              :color {:field "logFC"
                      :type "quantitative"
                      :aggregate "max"
                      :legend {:title nil}
                      }}})

(defn vegaspec-curve [points lines]
  ; data: [{:x # :y # :drug ""} ...]
  {:description "DR/Melt curves",
   :title (:prot (first points))
   :width 300
   :height 100
   :layer [{:data {:values points}
            :mark "point"
            :encoding {:x {:type "quantitative"
                           :field "x"
                           :scale {:zero false}
                           }
                       :y {:field "y"
                           :type "quantitative"}
                       :shape {:field "rep"
                               :type "nominal"}
                       :color {:field "drug"
                               :type "nominal"}}}
           {:data {:values lines} 
            :mark "line"
            :encoding {:x {:type "quantitative"
                           :field "x"
                           :scale {:zero false}
                           }
                       :y {:field "y"
                           :type "quantitative"}
                       :strokeDash {:field "rep"
                                    :type "nominal"}
                       :color {:field "drug"
                               :type "nominal"}}}]})

(defn vegaspec-volcano [data has-zero-pval?]
  ; data: [{:PValue # :logFC # :name ""} ...]
  (let [volc-spec
        {:encoding {:x {:type "quantitative"
                        :field "logFC"
                        ;:scale {:zero false}
                        }
                    :y {:field "sig"
                        :type "quantitative"
                        :scale {:zero false}
                        }
                    :color {:field "posfc"
                            :type "nominal"}
                    :shape {:field "rep"
                            :type "nominal"}
                    :tooltip [{:field "uniprot" :type "nominal"}
                              {:field "prot-name" :type "nominal"}]
                    }
         :mark "point"
         :width 600
         }
        views [(assoc volc-spec
                      :transform [{:filter "datum.PValue==0"} {:calculate "999" :as "sig"}]
                      :height 50)
               (assoc volc-spec
                      :transform [{:filter "datum.PValue>0"}]
                      :height 600
                      :width 600)]
        ]
    {:description "Volcano",
     :width 600
     :height 600
     :title (:drug (first data))
     :data {:values data}
     :vconcat (if has-zero-pval?
                views
                (rest views))
     :transform [{:calculate "-log(datum.PValue)/log(10)" :as "sig"}
                 {:calculate "if(datum.sig<2, \"insig\", datum.logFC>0)" :as "posfc"}]
     :config {:range {:category ["#5079a5" "#ef8e3b" "#bbbbbb"]}}}))

(defn s->ar [s] (map js/Number (str/split s ",")))

(defn make-points [{:keys [rep temperature_range concentration_range fold_change drugname uniprot prot-name]}]
  (let [x (s->ar (if (empty? concentration_range) temperature_range concentration_range))
        y (s->ar fold_change)
        ]
    (map (fn [x y] {:rep rep :pid uniprot :prot prot-name :drug drugname :x x :y y}) x y)))

(defn make-curve [{:keys [rep drugname uniprot prot-name pEC50 slope min_x max_x]}]
  (let [step 0.5]
    (for [x (range min_x (+ max_x step) step)]
      {:rep rep :pid uniprot :prot prot-name :drug drugname :x x :y (/ 1 (+ 1 (js/Math.exp (* (- (- pEC50) x) slope))))})))

(defn make-volcdata [{:keys [rep drugname uniprot prot-name prot-id fold_change pvalue]}]
  (let [fc (s->ar fold_change)
        fc (if (< (count fc) 3)
             [(fc)]
             [(apply min fc) (apply max fc)])]
    (map (fn [fc] {:rep rep :uniprot uniprot :prot-name prot-name :prot-id prot-id :drug drugname :PValue pvalue :logFC fc}) fc)))

(defn plotprot [[uniprot m]]
  (let [p (flatten (map make-points m))
        l (flatten (map make-curve m))]
    [vega-lite (vegaspec-curve p l)]))

(defn plotdrug [[drugid m]]
  (let [has-zero-pval? (some #(= (:pvalue %) 0) m)
        p (flatten (map make-volcdata m))]
    [vega-lite (vegaspec-volcano p has-zero-pval?)
     {:patch (fn [spec]
               (ocall spec [:signals :push]
                      (clj->js {:name "clickpoint"
                                :on [{:events "*:mousedown" :update "datum"}]}))
               spec)
      :callback (fn [result]
                  (ocall result [:view :addSignalListener]
                         "clickpoint" (fn [_ v] (let [pid (-> v
                                                              (js->clj :keywordize-keys true)
                                                              :prot-id
                                                              )]
                                                  (dispatch [::route/navigate
                                                             ::route/report {:type "Protein"
                                                                            :id pid}])))))}]))
(defn plotheat [r]
  (let [r (map (fn [x] (assoc x :logFC (last (s->ar (:fold_change x))))) r)]
    [vega-lite (vegaspec-heat r)]))

(defn heatmap []
  (let [hr @(subscribe [:cetsaresults/all-results])
        is-selected @(subscribe [:cetsaresults/filter-missing?])
        prot-intersect @(subscribe [:cetsaresults/prot-intersect])
        hr (if is-selected (filter #(get prot-intersect (:uniprot %)) hr) hr)
        ]
    [:div
   [:div 
    [:span "Filter missing?"]
    [:input
     {:type "checkbox"
      :checked is-selected
      :on-change #(dispatch [:cetsaresults/filter-missing (oget % :target :checked)])}]]
   [:div [plotheat hr]]]))

(defn proteins []
  [:div.grid-spaceBetween
   (if (or @(subscribe [:cetsaresults/no-curves?])
           (= :volcano @(subscribe [:cetsaresults/plot-type])))
     (for [r @(subscribe [:cetsaresults/by-drug])]
       ^{:key (key r)}
       [:div.col [plotdrug r]])
     (for [r @(subscribe [:cetsaresults/by-prot])]
       ^{:key (key r)}
       [:div.col [plotprot r]]
       ))])

(defn choose-plot-type []
  [:div
   [:button.btn.btn-raised.btn-default
    {:on-click #(dispatch [:cetsaresults/set-plot-type :curve])
     :disabled @(subscribe [:cetsaresults/no-curves?])}
    "Curve"]
   [:button.btn.btn-raised.btn-default
    {:on-click #(dispatch [:cetsaresults/set-plot-type :volcano])}
    "Volcano"]])

(defn main []
  [:div [:h2 "CETSA Results"]
   (if @(subscribe [:cetsaresults/fetching?])
    [mini-loader "middle"]
    [:div
     [drugs]
     [choose-plot-type]
     [proteins]
     [heatmap]
     ])]
  )
