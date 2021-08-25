(ns bluegenes.pages.projects.views
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]
            [bluegenes.components.icons :refer [icon icon-comp]]
            [bluegenes.components.loader :refer [mini-loader]]
            [oops.core :refer [oget oset!]]
            [goog.functions :refer [debounce]]
            [bluegenes.components.select-tags :as select-tags]
            [bluegenes.subs.auth :as auth]
            [clojure.string :as str]
            [bluegenes.route :as route]
            [bluegenes.components.bootstrap :refer [poppable]]
            [goog.string :as gstring])
  (:import goog.date.Date))


(defn filter-lists []
  (let [input (r/atom @(subscribe [:projects/keywords-filter]))
        debounced (debounce #(dispatch [:projects/set-keywords-filter %]) 500)
        on-change (fn [e]
                    (let [value (oget e :target :value)]
                      (reset! input value)
                      (debounced value)))]
    (fn []
      [:div.filter-lists
       [:h2 "Experiments"]
       [:div.filter-input
        [:input {:id "lists-keyword-filter"
                 :type "text"
                 :placeholder "Search for keywords"
                 :on-change on-change
                 :value @input}]
        [icon "search"]]])))

(defn bottom-controls []
  (let [list-count (count @(subscribe [:projects/selected-exp]))]
    (when (pos? list-count)
      [:div.bottom-controls
       [:div
        [:span.selected-indicator
         (str "Selected " list-count (cond-> " list" (> list-count 1) (str "s")))]
        [:button.btn.btn-raised.btn-default
         {:on-click #(dispatch [:projects/clear-selected])}
         "Deselect all"]]
       [:div
        [:button.btn.btn-raised.btn-info
         {:on-click #(dispatch [:projects/open-modal :move])}
         "Move all" [icon "new-folder"]]
        [:button.btn.btn-raised.btn-info
         {:on-click #(dispatch [:projects/open-modal :copy])}
         "Copy all" [icon "list-copy"]]
        [:button.btn.btn-raised.btn-danger
         {:on-click #(dispatch [:projects/open-modal :delete])}
         "Delete all" [icon "list-delete"]]]])))

(defn pagination-items [p pages]
  (concat
   [{:disabled (= p 1) :label "‹" :value (dec p)}]
   (when (> p 1) [{:label 1 :value 1}])
   (when (= p 3) [{:label 2 :value 2}])
   (when (> p 3) [{:label "..."}])
   (for [i (range p (min (inc pages) (+ p 3)))]
     {:active (= i p) :label i :value i})
   (when (< p (- pages 4)) [{:label "..."}])
   (when (= p (- pages 4)) [{:label (dec pages) :value (dec pages)}])
   (when (< p (- pages 2)) [{:label pages :value pages}])
   [{:disabled (= p pages) :label "›" :value (inc p)}]))

(defn pagination []
  (let [per-page @(subscribe [:projects/per-page])
        page-count @(subscribe [:projects/page-count])
        current-page @(subscribe [:projects/current-page])]
    ;; Don't show pagination if there are no pages and therefore no lists.
    (when (pos? page-count)
      ;; This is a guard to switch to the last page if the current one no
      ;; longer exists. The right place to put it would be in :projects/initialize
      ;; but we have to normalize-lists to tell the page count. As a future
      ;; refactoring improvement, you could compute page count as part of
      ;; denormalization and put it in app-db, instead of using it as a sub.
      (when (> current-page page-count)
        (dispatch [:projects/set-current-page page-count]))
      [:div.pagination-controls
       [:span "Rows per page"]
       [:div.dropdown
        [:button.btn.btn-raised.dropdown-toggle.rows-per-page
         {:data-toggle "dropdown"}
         per-page [icon "caret-down"]]
        (into [:ul.dropdown-menu]
              (map (fn [value]
                     [:li {:class (when (= value per-page) :active)}
                      [:a {:on-click #(dispatch [:projects/set-per-page value])}
                       value]])
                   [20 50 100]))]
       (into [:ul.pagination]
             (map (fn [{:keys [disabled active label value]}]
                    [:li {:class (cond disabled :disabled
                                       active :active)}
                     [:a {:disabled disabled
                          :on-click (when (and (not disabled) value)
                                      #(dispatch [:projects/set-current-page value]))}
                      label]])
                  (pagination-items current-page page-count)))])))

(defn pagination-bottom []
  (let [page-count @(subscribe [:projects/page-count])
        current-page @(subscribe [:projects/current-page])]
    ;; Don't show pagination if there are no pages and therefore no lists.
    (when (pos? page-count)
      [:div.pagination-controls.pagination-bottom
       (into [:ul.pagination]
             (map (fn [{:keys [disabled active label value]}]
                    [:li {:class (cond disabled :disabled
                                       active :active)}
                     [:a {:disabled disabled
                          :on-click (when (and (not disabled) value)
                                      #(dispatch [:projects/set-current-page value true]))}
                      label]])
                  (pagination-items current-page page-count)))])))

(let [minute 60000
      hour 3.6e+6
      day 8.64e+7]
  (defn pretty-timestamp [ts]
    (let [now (.getTime (js/Date.))
          ago (- now ts)
          today (.getTime (Date.)) ; Uses goog.date.Date for midnight time.
          yesterday (- today day)]
      (cond
        (< ago minute) "Just now"
        (< ago hour) (let [amount (quot ago minute)]
                       (str amount " min" (when (> amount 1) "s") " ago"))
        (>= ts today) (let [amount (quot ago hour)]
                        (str amount " hour" (when (> amount 1) "s") " ago"))
        (>= ts yesterday) "Yesterday"
        :else ts))))

(defn readable-time [ts]
  [poppable {:data (str ts " 00:00:00")
             :children (pretty-timestamp ts)}])

(defn sort-button [column]
  (let [active-sort @(subscribe [:projects/sort])]
    [:button.btn
     {:on-click #(dispatch [:projects/toggle-sort column])}
     [icon "sort" nil [(when (= column (:column active-sort))
                         (case (:order active-sort)
                           :asc "active-asc-sort"
                           :desc "active-desc-sort"))]]]))

(defn selection-button [filter-name items]
  (let [active-value @(subscribe [:projects/filter filter-name])]
    [:div.dropdown
     [:button.btn.dropdown-toggle
      {:data-toggle "dropdown"}
      [icon "selection" nil [(when (some? active-value)
                               "active-selection")]]]
     (into [:ul.dropdown-menu]
           (for [{:keys [label value]} items]
             [:li {:class (when (= value active-value)
                            "active")}
              [:a {:on-click #(dispatch [:projects/set-filter filter-name value])}
               label]]))]))

(defn list-row-controls [{:keys [id authorized status name]}]
  [:<>
   (when (= status "TO_UPGRADE")
     [:a.btn
      {:href (route/href ::route/upgrade nil {:name name})}
      [icon "arrow-up"]])
   [:button.btn
    {:on-click #(dispatch [:projects/open-modal :copy id])
     :disabled (= status "TO_UPGRADE")}
    [icon "list-copy"]]
   [:button.btn
    {:on-click #(dispatch [:projects/open-modal :edit id])
     :disabled (not authorized)}
    [icon "list-edit"]]
   [:button.btn
    {:on-click #(dispatch [:projects/open-modal :delete id])
     :disabled (not authorized)}
    [icon "list-delete"]]])

(defn exp-row [item]
  (let [{:keys [id title size private description run_date experiment_type project_name is-last]} item
        selected-exp @(subscribe [:projects/selected-exp])
        new-lists @(subscribe [:projects/new-lists])
        is-selected (contains? selected-exp id)
        is-new (contains? new-lists {:id id})]
    [:div.lists-row.lists-item
     {:class (when is-last :separator)}

     [:div.lists-col
      [:input {:type "checkbox"
               :checked is-selected
               :on-change #(dispatch [(if (oget % :target :checked)
                                        :projects/select-list
                                        :projects/deselect-list)
                                      id])}]
      [:span.list-icon
       {:class (when is-new :new)}
       (if is-new
         [poppable {:data "This list was just created by you."
                    :children [icon "list-item"]}]
         [icon "list-item"])]]

     [:div.lists-col
      [:div.list-detail
       [:a.list-title
        {:href (route/href ::route/cetsaresults {:id id})}
        title]
       [:span.list-size (str "[" size "]")]
       (if private
         [poppable {:data "This experiment is private."
                    :children [icon "user-circle" nil ["authorized"]]}]
         [poppable {:data "This experiment is public"
                    :children [icon "globe"]}])]
      [:p.list-description description]]

     [:div.lists-col
      [:span.list-timestamp
       [readable-time run_date]]]

     [:div.lists-col
      [:code.start {:class (str "start-" experiment_type)}
       experiment_type]]

     [:div.lists-col
      (into [:div.list-project-name]
            ;; Hide internal tags.
            (for [n [project_name]]
              [:code.tag n]))]

     [:div.lists-col.vertical-align-cell
      [:<>
       [:div.list-controls.hidden-lg
        [:div.dropdown
         [:button.btn.dropdown-toggle
          {:data-toggle "dropdown"}
          [icon "list-more"]]
         [:div.dropdown-menu.dropdown-menu-controls
          [:div.list-controls
           [list-row-controls item]]]]]
       [:div.list-controls.hidden-xs.hidden-sm.hidden-md
        [list-row-controls item]]]
      (when is-selected
        [:div.selected-list-overlay])]]))

(defn lists []
  (let [filtered-exp @(subscribe [:projects/filtered-exp])
        exp-selection @(subscribe [:projects/filter :experiments])
        all-types @(subscribe [:projects/all-types])
        all-projects @(subscribe [:projects/all-projects])
        all-selected? @(subscribe [:projects/all-selected?])
        new-hidden-lists @(subscribe [:projects/new-hidden-lists])
        fetching-exp? @(subscribe [:projects/fetching?])]
    [:section.lists-table

     [:header.lists-row.lists-headers
      [:div.lists-col
       [:input {:type "checkbox"
                :checked all-selected?
                :on-change (if all-selected?
                             #(dispatch [:projects/clear-selected])
                             #(dispatch [:projects/select-all-lists]))}]]
      [:div.lists-col
       [:div.list-header
        [:span (str "Experiment details ("
                    (case exp-selection
                      nil "All"
                      :private "Private only"
                      :public "Public only")
                    ")")]
        [sort-button :title]
        [selection-button
         :experiments
         [{:label "All" :value nil}
          {:label "Private only" :value :private}
          {:label "Public only" :value :public}]]]]
      [:div.lists-col
       [:div.list-header
        [:span "Date"]
        [sort-button :run_date]
        [selection-button
         :run_date
         [{:label "All" :value nil}
          {:label "Last day" :value :day}
          {:label "Last week" :value :week}
          {:label "Last month" :value :month}
          {:label "Last year" :value :year}]]]]
      [:div.lists-col
       [:div.list-header
        [:span "Type"]
        [sort-button :experiment_type]
        [selection-button
         :experiment_type
         (cons {:label "All" :value nil}
               (map (fn [type] {:label type :value type}) all-types))]]]
      [:div.lists-col
       [:div.list-header
        [:span "Project"]
        [sort-button :project_name]
        [selection-button
         :project_name
         (cons {:label "All" :value nil}
               (map (fn [proj] {:label proj :value proj}) all-projects))]]]
      [:div.lists-col
       [:div.list-header
        [:button.btn.refresh-button
         {:disabled fetching-exp?
          :on-click #(dispatch [:projects/get-experiments])}
         (if fetching-exp?
           [mini-loader "tiny"]
           [poppable {:data "Refresh lists"
                      :children [icon-comp "refresh"]}])]]]]

     (when (seq new-hidden-lists)
       (let [amount (count new-hidden-lists)
             plural? (> amount 2)]
         [:div.lists-row
          [:div.new-lists-alert.text-center
           (str "You have " amount " new list"
                (when plural? "s")
                " that "
                (if plural? "aren't" "isn't")
                " visible under the active filters. ")
           [:a {:role "button"
                :on-click #(dispatch [:projects/show-new-lists])}
            "Click here"]
           " to clear filters."]
          ;; There's no colspan with CSS Tables so we use the dummy element
          ;; below to occupy space, and absolute positioning for the above.
          [:div.new-lists-dummy (gstring/unescapeEntities "&nbsp;")]]))

     (for [{:keys [id] :as item} filtered-exp]
       ^{:key id}
       [exp-row item])]))

(defn no-lists []
  (let [no-lists? @(subscribe [:projects/no-exp?])
        no-filtered-lists? @(subscribe [:projects/no-filtered-exp?])
        is-empty (or no-lists? no-filtered-lists?)
        mine-name @(subscribe [:local-mine-human-name])]
    (when is-empty
      [:div.no-lists
       (cond
         no-lists? [:h3 (str mine-name " has no public experiments available")]
         no-filtered-lists? [:h3 "No experiment matches active filters"])
       [:hr]
       [:p "There may be private experiments available. Lab members, login to access them."]
       [:hr]
       (when-not no-lists?
         [:p [:a {:role "button"
                  :on-click #(dispatch [:projects/reset-filters])}
              "Click here"]
          " to clear all filters."])])))

(defn main []
  [:div.container-fluid.lists
   [filter-lists]
   [pagination]
   [lists]
   [no-lists]
   [pagination-bottom]
   [bottom-controls]])
