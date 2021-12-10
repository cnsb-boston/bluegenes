(ns bluegenes.pages.reportpage.views
  (:require [re-frame.core :as re-frame :refer [subscribe dispatch]]
            [reagent.core :as r]
            [bluegenes.pages.reportpage.components.toc :as toc]
            [bluegenes.pages.reportpage.components.sidebar :as sidebar]
            [bluegenes.pages.reportpage.utils :as utils :refer [description-dropdown]]
            #_[bluegenes.components.table :as table]
            [bluegenes.components.lighttable :as lighttable]
            [bluegenes.components.loader :refer [loader mini-loader]]
            [bluegenes.components.tools.views :as tools]
            [bluegenes.pages.reportpage.events :as events]
            [bluegenes.pages.reportpage.subs :as subs]
            [bluegenes.pages.cetsaresults.events]
            [im-tables.views.core :as im-table]
            [bluegenes.route :as route]
            [bluegenes.components.viz.views :as viz]
            [bluegenes.components.icons :refer [icon icon-comp]]
            [clojure.string :as str]
            [bluegenes.components.bootstrap :refer [poppable]]
            [bluegenes.utils :refer [encode-file]]
            [oops.core :refer [ocall oget]]
            [cljsjs.react-table]
            [goog.functions :refer [debounce]]))

(def ReactTable (aget js/ReactTable "default"))

(defn tbl [{:keys [loc collapse]}]
  (let [data (subscribe [::subs/a-table loc])
        is-collapsed* (r/atom collapse)]
    (r/create-class
     {:component-did-mount (fn [this]
                             (let [{:keys [loc] :as props} (r/props this)]
                               (when-not @is-collapsed*
                                 (dispatch [:im-tables/load loc (dissoc props :loc)]))))
      :reagent-render (fn [{:keys [loc title id description] :as props}]
                        ;; result-count will be `nil` if query hasn't been loaded.
                        (let [result-count (get-in @data [:response :iTotalRecords])]
                          [:div.report-item
                           {:class [(when (or @is-collapsed* (zero? result-count))
                                      :report-item-collapsed)
                                    (when (zero? result-count)
                                      :report-item-no-results)]
                            :id id}
                           [:h4.report-item-heading
                            {:on-click (fn []
                                         ;; Run query if it hasn't been run due to being collapsed.
                                         (when (and (nil? result-count) @is-collapsed*)
                                           (dispatch [:im-tables/load loc (dissoc props :loc)]))
                                         (swap! is-collapsed* not))}
                            [:span.report-item-title
                             (str title (when result-count (str " (" result-count ")")))
                             (when description [description-dropdown description])]
                            (when ((some-fn nil? pos?) result-count)
                              [:span.report-item-toggle
                               (if @is-collapsed*
                                 [icon "expand-folder"]
                                 [icon "collapse-folder"])])]
                           (cond
                             @is-collapsed* nil
                             (zero? result-count) nil
                             :else [:div
                                    [im-table/main loc]])]))})))

(defn ->report-table-settings [current-mine-name]
  {:pagination {:limit 5}
   :compact true
   :links {:vocab {:mine (name (or current-mine-name ""))}
           :url (fn [{:keys [mine class objectId] :as vocab}]
                  (route/href ::route/report
                              {:mine mine
                               :type class
                               :id objectId}))}})

(defn tool-report [{:keys [collapse id description] tool-cljs-name :value}]
  (let [tool-details @(subscribe [::subs/a-tool tool-cljs-name])
        ;; The line below relies on the fact that a report page is always one entity.
        ;; If there could be multiple, we'd have to use `utils/suitable-entities`.
        entity @(subscribe [:bluegenes.components.tools.subs/entities])]
    [tools/tool (assoc tool-details :entity entity)
     :collapse collapse
     :id id
     :description description]))

(defn template-report [{:keys [id collapse description] template-name :value}]
  (let [summary-fields @(subscribe [:current-summary-fields])
        service (:service @(subscribe [:current-mine]))
        current-mine-name @(subscribe [:current-mine-name])
        {:keys [title] :as template} @(subscribe [::subs/a-template template-name])]
    [tbl {:loc [:report :im-tables id]
          :service (merge service {:summary-fields summary-fields})
          :title title
          :collapse collapse
          :description description
          :query template
          :settings (->report-table-settings current-mine-name)
          :id id}]))

(defn class-report [{:keys [id collapse description] nom :value}]
  (let [{object-type :type object-id :id} @(subscribe [:panel-params])
        summary-fields @(subscribe [:current-summary-fields])
        service (:service @(subscribe [:current-mine]))
        current-mine-name @(subscribe [:current-mine-name])
        {:keys [displayName] :as ref+coll} @(subscribe [::subs/a-ref+coll nom])]
    [tbl {:loc [:report :im-tables id]
          :service (merge service {:summary-fields summary-fields})
          :title displayName
          :collapse collapse
          :description description
          :query (utils/->query-ref+coll summary-fields object-type object-id ref+coll)
          :settings (->report-table-settings current-mine-name)
          :id id}]))

(defn fetch-res-data [report id]
  (dispatch [:cetsaresults/get-results-by report id]))

(defn cetsarow-click-fn [target-class state rowInfo instance]
  (clj->js {:onClick #(dispatch [::route/navigate
                                ::route/report {:type target-class
                                                :id (aget rowInfo "row" "objectId")}])}))

(defn cetsarow-search-fn [kw state rowInfo instance]
  (clj->js {:onClick #(when-let [kw-v (aget rowInfo "row" kw)]
                        (dispatch [::route/navigate
                                   ::route/search nil {:keyword kw-v}]))}))

(defn make-report [id {:keys [fetch col link]}]
  (let [crep @(subscribe [::subs/report-cetsa])]
    (fetch-res-data fetch id)
    [:div
     (if (empty? crep)
       "None / Loading"
       [:> ReactTable {:data crep :columns col
                       :pageSize (min (count crep) 5)
                       :getTrProps link}])]))

(defn cetsa-report []
  (let [{:keys [rootClass]} @(subscribe [::subs/report-summary])
        cid @(subscribe [::subs/report-cetsa-dbid])
        drugtypes #{"DrugCompound" "PDBCompound" "ChemblCompound" "KeggCompound" "PubChemCompound"}]
    (condp some  #{rootClass}
      #{"Protein"}
      [make-report cid {:fetch "protein"
                        :col [{:Header "DrugName" :accessor "drugname"}
                              {:Header "drugBankId" :accessor "drugbankID"}
                              {:Header "InChIKey" :accessor "inchikey"}]
                        :link (partial cetsarow-search-fn "inchikey")}]
      drugtypes
      [make-report cid {:fetch "drug"
                        :col [{:Header "ProteinName" :accessor "name"}
                              {:Header "Uniprot" :accessor "primaryAccession"}
                              {:Header "objectId" :accessor "objectId" :show false}]
                        :link (partial cetsarow-click-fn "Protein")}]
      [:div (str "Valid report types for CETSA: Protein, " (str/join ", " drugtypes))])))

(defn section []
  (let [collapsed* (r/atom false)]
    (fn [{:keys [title id]} & children]
      (into [:div.report-table {:id id}
             [:h3.report-table-heading
              title
              [:button.btn.btn-link.pull-right.collapse-table
               {:on-click #(swap! collapsed* not)}
               [icon-comp "chevron-up"
                :classes [(when @collapsed* "collapsed")]]]]]
            (when-not @collapsed* children)))))

(defn report []
  (let [{:keys [rootClass]} @(subscribe [::subs/report-summary])
        categories @(subscribe [:current-mine/report-layout rootClass])
        filter-text @(subscribe [::subs/report-filter-text])]
    [:div
     (doall
      (for [{:keys [category id children]} categories
            ;; This might seem heavy, but most of the signal graph is already
            ;; cached due to `:current-mine/report-layout` subscribing to the
            ;; fallback layout.  What this does is make sure report item is
            ;; "available" for this class (mostly to avoid showing parts of the
            ;; default layout that doesn't apply to this class).
            :let [children (filter (fn [{:keys [type value] :as child}]
                                     (and (contains? (case type
                                                       "class"    @(subscribe [::subs/ref+coll-for-class? rootClass])
                                                       "template" @(subscribe [::subs/template-for-class? rootClass])
                                                       "tool"     @(subscribe [::subs/tool-for-class? rootClass]))
                                                     value)
                                          (if (not-empty filter-text)
                                            (let [label (toc/parse-item-name child)]
                                              (str/includes? (str/lower-case label) (str/lower-case filter-text)))
                                            true)))
                                   children)]
            :when (seq children)] ; No point having a section without children.
        ^{:key id}
        [section
         {:title category
          :id id}
         [:div
          (doall
           (for [{:keys [_label _value type _collapse _description id] :as child} children
                 :let [report-comp (case type
                                     "class"    class-report
                                     "template" template-report
                                     "tool"     tool-report)]]
             ^{:key id}
             [report-comp (assoc child :collapse true)]))]]))]))

(defn chemimg [imgsrc]
  [:<>
   [:div
    [:img {:src imgsrc :height 250}]]])

(defn summary-location [[label value]]
  [:<>
   [:div.report-table-cell.report-table-header
    label]
   [:div.report-table-cell
    [:span.report-table-link
     {:on-click #(dispatch [::events/open-in-region-search value])}
     [poppable {:data "Perform a search of this region"
                :children value}]]]])

(defn fasta-download []
  (let [id           (subscribe [::subs/fasta-identifier])
        fasta        (subscribe [::subs/fasta])
        download-ref (atom nil)
        download!    #(let [el @download-ref
                            url (encode-file @fasta "fasta")]
                        (ocall el :setAttribute "href" url)
                        (ocall el :setAttribute "download" (str @id ".fasta"))
                        (ocall el :click)
                        (ocall js/window.URL :revokeObjectURL url))]
    (fn []
      [:<>
       [:a.hidden-download {:download "download" :ref (fn [el] (reset! download-ref el))}]
       [:a.fasta-download {:role "button" :on-click download!}
        [icon-comp "download"]
        "FASTA"]])))

(defn summary-fasta [[label value]]
  (let [fasta @(subscribe [::subs/fasta])]
    [:<>
     [:div.report-table-cell.report-table-header
      label]
     (case fasta
       :fasta/fetch
       [:div.report-table-cell.fasta-value
        [mini-loader "tiny"]]

       :fasta/none
       [:div.report-table-cell.fasta-value
        [:a.fasta-download.disabled
         {:role "button"
          :disabled true}
         [icon-comp "my-data"]
         "NOT AVAILABLE"]]

       :fasta/long
       (let [{:keys [mine type id]} @(subscribe [:panel-params])]
         [:div.report-table-cell.fasta-value
          [:a.fasta-download
           {:role "button"
            :on-click #(dispatch [:fetch-fasta (keyword mine) type id])}
           [icon-comp "my-data"]
           "LOAD FASTA"]])

       ;; In this branch, fasta will be a string containing the fasta.
       [:div.report-table-cell.fasta-value
        [:span.dropdown
         [:a.dropdown-toggle.fasta-button
          {:data-toggle "dropdown" :role "button"}
          [poppable {:data "Show sequence"
                     :children [:span value [icon-comp "caret-down"]]}]]
         [:div.dropdown-menu.fasta-dropdown
          [:form ; Top secret technique to avoid closing the dropdown when clicking inside.
           [:pre.fasta-sequence fasta]]]]
        [fasta-download]])]))

(defn anchor-if-url [x]
  (if (string? x)
    (let [s (str/trim x)]
      (if (re-matches #"https?://[^\s]*" s)
        [:a {:href s :target "_blank"} s]
        x))
    x))

(defn summary []
  (let [{:keys [columnHeaders results]} @(subscribe [::subs/report-summary])
        fasta               @(subscribe [::subs/fasta])
        chromosome-location @(subscribe [::subs/chromosome-location])
        fasta-length        @(subscribe [::subs/fasta-length])
        entries (->> (concat (sort-by key (filter val (zipmap columnHeaders (first results))))
                             (when (not-empty chromosome-location)
                               [^{:type :location} ["Chromosome Location" chromosome-location]])
                             (when fasta
                               [^{:type :fasta} ["Sequence Length" fasta-length]]))
                     (partition-all 2))]
    [section
     {:title utils/pre-section-title
      :id utils/pre-section-id}
     (into [:div.report-table-body]
           (for [row entries]
             (into [:div.report-table-row]
                   (for [cell row
                         :let [meta-type (-> cell meta :type)]]
                     (case meta-type
                       :location [summary-location cell]
                       :fasta [summary-fasta cell]
                       [:<>
                        [:div.report-table-cell.report-table-header
                         (-> cell key (str/split " > ") last)]
                        [:div.report-table-cell
                         (-> cell val (or "N/A") anchor-if-url)]])))))
     [:div.hidden-lg.sidebar-collapsed
      [sidebar/main]]]))

(defn filter-input []
  (let [input (r/atom @(subscribe [::subs/report-filter-text]))
        debounced (debounce #(dispatch [::events/set-filter-text %]) 500)
        on-change (fn [e]
                    (let [value (oget e :target :value)]
                      (reset! input value)
                      (debounced value)))]
    (fn []
      [:div.report-page-filter
       [poppable {:data "This will search for keywords in the section titles, use the browser search to find text anywhere in the page."
                  :children [icon-comp "info"]}]
       [:input.form-control
        {:type "text"
         :placeholder "Topic filter"
         :on-change on-change
         :value @input}]])))

(defn heading []
  (let [{:keys [rootClass]} @(subscribe [::subs/report-summary])
        title @(subscribe [::subs/report-title])]
    [:h1.report-page-heading
     title
     [:code.start {:class (str "start-" rootClass)} rootClass]]))

(defn invalid-object []
  (let [{error-type :type ?message :message} @(subscribe [::subs/report-error])]
    [:div.row
     [:div.col-xs-8.col-xs-offset-2
      [:div.well.well-lg.invalid-object-container
       [:h2 (case error-type
              :not-found "Object not found"
              :ws-failure "Failed to retrieve object")]
       [:p (case error-type
             :not-found "It may have existed before and been assigned a new ID after a database rebuild. If you remember one of its identifiers, you can search for it. You can also see if any lists contain it. Please export a permanent URL next time you want to keep a link to an object."
             :ws-failure "This may be due to a network error, invalid URL or server issues. Please verify that the URL is correct and try again later.")]
       (when-let [msg (not-empty ?message)]
         [:pre msg])
       [:a.btn.btn-primary.btn-lg
        {:href (route/href ::route/home)}
        "Go to homepage"]]]]))

(defn main []
  (let [fetching-report? @(subscribe [:fetching-report?])
        error @(subscribe [::subs/report-error])
        params @(subscribe [:panel-params])]
    [:div.container-fluid.report-page
     (cond
       fetching-report? [loader (str (:type params) " Report")]
       error [invalid-object]
       :else [:<>
              [:div.row
               [:div.col-xs-2
                [filter-input]]
               [:div.col-xs-8
                [heading]]]
              [:div.row.report-row
               [:div.col-xs-2
                [toc/main]]
               [:div.col-xs-10.col-lg-8
                [summary]
                (when-let [imgsrc @(subscribe [::subs/report-mol-img])]
                  [section {:title "Structure" :id "Structure"} [chemimg imgsrc]])
                [section {:title "CETSA" :id "CETSA"} [cetsa-report]]
                [report]]
               [:div.col-lg-2.visible-lg-block.sidebar-container
                [sidebar/main]]]])]))
