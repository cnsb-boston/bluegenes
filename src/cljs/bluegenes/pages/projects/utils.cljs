(ns bluegenes.pages.projects.utils
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [goog.date :refer [fromIsoString]]
            )
  (:import goog.date.Date))

(defn denormalize-lists
  "Takes a sequential of experiment maps, as delivered by the webservice, returning
  a map from experimentIDs, to experiment map."
  [lists]
  (->> lists
       (map #(set/rename-keys % {:experimentID :id}))
       (map (fn [x] (assoc x
                           :title (str/join " - " (vals (select-keys x [:done_by :organism :num_samples :num_replicates :assay_type])))
                           :timestamp (.getTime (fromIsoString (:run_date x)))
                           :private (pos? (:private x))
                           )))     
       (zipmap (map :experimentID lists))))


(defn normalize-lists
  "Takes the denormalized experiment data and converts it into a flat sequence
  of maps for rendering. Takes functions to filter and sort, which should be
  created with the helpers below. Optionally takes pagination data."
  [filterf sortf {:keys [by-id]}
   & [{:keys [per-page current-page] :as pagination}]]
  (let [top-level-maps (vals by-id)]
    (cond-> (sortf
              (filterf top-level-maps))
       pagination (->> (drop (* per-page (dec current-page)))
                       (take per-page)))))

(defn ->filterf
  "Create a filter function from the filters map in controls, to be passed to
  `normalize-lists`. Remember that comp's arguments are run in reverse order!
  Will only be passed list maps, and not folders."
  [{:keys [keywords experiments run_date experiment_type project_name] :as _filterm}]
  (comp
   ;; Keyword filter should be done last.
   (if (empty? keywords)
     identity
     (let [keyws (map str/lower-case (-> keywords str/trim (str/split #"\s+")))]
       ;; Slightly faster; consider it if you wish to improve performance.
       #_(partial filter (fn [{:keys [title description]}]
                           (let [s (-> (str title " " description)
                                       (str/lower-case))]
                             (every? #(str/includes? s %) keyws))))
       ;; The following function filters by matching all the different fields
       ;; belonging to a list. Performance seems quite good even for 200 lists.
       (partial filter (fn [listm]
                         (let [all-text (->> listm
                                             ((juxt :title :size :description :type
                                                    ;; Note that internal tags aren't removed!
                                                    (comp #(str/join " " %) :tags)))
                                             (str/join " ")
                                             (str/lower-case))]
                           (every? #(str/includes? all-text %) keyws))))))
   ;; Filter by project name. 
   (if (nil? project_name)
     identity
     (partial filter (comp #(contains? % project_name) set :project_name)))
   ;; Filter by type.
   (if (nil? experiment_type)
     identity
     (partial filter (comp #{experiment_type} :experiment_type)))
   ;; Filter by details.
   (if (nil? experiments) ; Folders first handled in `->sortf`.
     identity
     (partial filter (case experiments
                       :private (comp true? :private)
                       :public (comp false? :private))))
   ;; Filter by date.
   (if (nil? run_date)
     identity
     ;; This uses `goog.date.Date` instead of `js/Date`. The main difference is
     ;; that the former defaults to midnight of the current date, while the
     ;; latter defaults to the current time in UTC.
     (let [now (.getTime (Date.))]
       (partial filter (case run_date
                         :day   (comp #(> (+ % 8.64e+7) now) :timestamp)
                         :week  (comp #(> (+ % 6.048e+8) now) :timestamp)
                         :month (comp #(> (+ % 2.628e+9) now) :timestamp)
                         :year  (comp #(> (+ % 3.154e+10) now) :timestamp)))))))

(defn ->sortf
  "Create a sort function from the sort map in controls, to be passed to
  `normalize-lists`. Remember that comp's arguments are run in reverse order!
  Will be passed both list and folder maps."
  [{:keys [column order] :as _sortm} & {:keys [folders-first?]}]
  (comp
   ;; Show private lists first.
   (partial sort-by :authorized (comp - compare))
   ;; Sort according to active control.
   (partial sort-by
            ;; We don't want "B" to come before "a", so we lowercase strings.
            (comp #(cond-> % (string? %) str/lower-case)
                  column)
            ;; `compare` also works great for vectors, for which it will first
            ;; sort by length, then by each element.
            (case order
              :asc compare
              :desc (comp - compare)))))

(defn filtered-list-ids-set
  "Returns a set of all list IDs that are selectable with the currently active
  filters. This is needed when we want to select all lists, or check if all
  lists are currently selected."
  [items-by-id active-filters]
  (->> (normalize-lists
        (->filterf active-filters)
        identity
        {:by-id items-by-id})
       (map :id)
       (set)))

(defn copy-list-name
  "Returns a new unique list name to be used for a copied list. Usually this
  is achieved by appending '_1', but as there might already exist a list with
  this name, we instead find the identical list names with a '_*' postfix and
  grab the one with the highest number. The increment of this number will then
  be used as postfix for the new list name."
  [by-id list-name]
  (let [greatest-postfix-num (->> (vals by-id)
                                  (map :name)
                                  (filter #(str/starts-with? % (str list-name "_")))
                                  (map #(-> % (str/split #"_") peek js/parseInt))
                                  (apply max))
        ;; We need to handle nil (no lists with postfix) and NaN (empty after postfix).
        new-postfix (if (or (not (number? greatest-postfix-num))
                            (.isNaN js/Number greatest-postfix-num))
                      1
                      (inc greatest-postfix-num))]
    (str list-name "_" new-postfix)))
