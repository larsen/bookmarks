(ns ^:figwheel-hooks bookmarks-frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [cljs.core]
   [clojure.set]
   [clojure.string]
   [goog.dom :as gdom]
   [reagent-material-ui.colors :as colors]
   [reagent-material-ui.core.app-bar :refer [app-bar]]
   [reagent-material-ui.core.chip :refer [chip]]
   [reagent-material-ui.core.container :refer [container]]
   [reagent-material-ui.core.css-baseline :refer [css-baseline]]
   [reagent-material-ui.core.grid :refer [grid]]
   [reagent-material-ui.core.button :refer [button]]
   [reagent-material-ui.core.icon-button :refer [icon-button]]
   [reagent-material-ui.core.input-base :refer [input-base]]
   [reagent-material-ui.core.paper :refer [paper]]
   [reagent-material-ui.core.card :refer [card]]
   [reagent-material-ui.core.card-header :refer [card-header]]
   [reagent-material-ui.core.card-content :refer [card-content]]
   [reagent-material-ui.core.card-actions :refer [card-actions]]
   [reagent-material-ui.core.toolbar :refer [toolbar]]
   [reagent-material-ui.core.typography :refer [typography]]
   [reagent-material-ui.icons.more-vert :refer [more-vert]]
   [reagent-material-ui.icons.search :refer [search]]
   [reagent-material-ui.styles :as styles]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [keybind.core :as key]))

(defonce app-state
  (atom {:bookmarks []
         :tags []
         :search-filter ""
         :focused-bookmark nil
         :tag-filter #{}}))

(defonce focused-bookmark-idx (atom 0))

(defn get-bookmarks []
  (go (let [response (<! (http/get "http://localhost:5000/bookmarks"
                                   {:with-credentials? false}))]
        (swap! app-state assoc :bookmarks (:body response)))))

(defn get-tags []
  (go (let [response (<! (http/get "http://localhost:5000/tags"
                                   {:with-credentials? false}))]
        (swap! app-state assoc :tags (:body response)))))

(def custom-theme
  {:spacing 8
   :palette {:primary   colors/teal
             :secondary colors/green}})

(defn custom-styles [{:keys [spacing] :as theme}]
  {:root {:flexGrow 1}
   :search {:position "relative"
            :backgroundColor "yellow"
            :marginLeft 0
            :width "100%"
            }
   :searchIcon {:padding "0 2"
                :height "100%"
                :position "absolute"
                :pointerEvents "none"
                :display "flex"
                :alignItems "center"
                :justifyContent "center"}
   :button     {:margin (spacing 1)}
   :text-field {:width        200
                :margin-left  (spacing 1)
                :margin-right (spacing 1)}})

(def with-custom-styles (styles/with-styles custom-styles))

(defn event-value
  [^js/Event e]
  (.. e -target -value))

(defn short-printable-url [url]
  ;; FIXME Should I use styles instead?
  (take 50 url))

(defn toggle-tag-filter [tag-name]
  (let [tag-filter (:tag-filter @app-state)]
    (swap! app-state assoc :tag-filter
           (if (contains? tag-filter tag-name)
             (clojure.set/difference tag-filter #{tag-name})
             (clojure.set/union tag-filter #{tag-name})))))

(defn has-tags [bookmark tags-set]
  (let [tag-names-set (set (map :name (:tags bookmark)))]
    (seq (clojure.set/intersection tag-names-set tags-set))))

(defn has-filter-tags [bookmark]
  (has-tags bookmark (:tag-filter @app-state)))

(defn tag-count-in-bookmarks [tag]
  (count (filter #(has-tags % #{(:name tag)}) (:bookmarks @app-state))))

(defn tag-component [tag & {:keys [with-count]}]
  (let [tag-name (:name tag)]
    [chip {:key (gensym "tag-")
           :size "small"
           :label (if with-count
                    (clojure.string/join
                     " " [tag-name (str (tag-count-in-bookmarks tag))])
                    tag-name)
           :color (if (contains? (:tag-filter @app-state) tag-name)
                    "secondary"
                    "primary")
           :on-click (fn [_] (toggle-tag-filter tag-name))}]))

(defn tags-list []
  (reagent/create-class {:component-did-mount (fn [] (get-tags))
                         :reagent-render (fn []
                                           [grid
                                            (doall (for [tag (:tags @app-state)]
                                                     (tag-component tag :with-count true)))])}))

(defn bookmark-component [counter bookmark]
  [card {:key (gensym "bookmark-")
         :variant "outlined"}
   [card-header {:title (:description bookmark)}]
   [card-content
    [typography {:variant "body2"}
     [:a {:href (:url bookmark)} (short-printable-url (:url bookmark))]]
    (when (= counter @focused-bookmark-idx)
      [typography {:variant "caption"} "current"])]
   [card-actions
    [button "Link"]
    (doall (for [tag (:tags bookmark)]
             (tag-component tag)))]])

(defn filter-by-tags [bookmarks]
  (let [tag-filter (:tag-filter @app-state)]
    (if (seq tag-filter)
      (seq (filter #(has-filter-tags %) bookmarks))
      bookmarks)))

(defn bookmark-matches-filter [bookmark search-filter]
  (if (not-empty search-filter)
    (let [filter-re (re-pattern (clojure.string/join "" ["(?i)" search-filter]))]
      (or (not-empty (cljs.core/re-find filter-re (:description bookmark)))
          (not-empty (cljs.core/re-find filter-re (:url bookmark)))))
    true))

(defn filter-by-string [bookmarks]
  (filter #(bookmark-matches-filter % (:search-filter @app-state)) bookmarks))

(defn visible-bookmarks []
  (-> (:bookmarks @app-state)
      filter-by-tags
      filter-by-string))

(defn focus-next-bookmark []
  ;; FIXME Needs to take into account
  ;; the actual size of the bookmarks
  ;; collection visible to the user!
  (swap! focused-bookmark-idx inc)
  ;; FIXME slow
  (swap! app-state assoc :focused-bookmark (nth (:bookmarks @app-state) @focused-bookmark-idx)))

(defn focus-prev-bookmark []
  (when (> @focused-bookmark-idx 0)
    (swap! focused-bookmark-idx dec)
    (swap! app-state assoc :focused-bookmark (nth (:bookmarks @app-state) @focused-bookmark-idx))))

(defn bookmarks-list []
  (reagent/create-class
   {:component-did-mount
    (fn []
      (key/bind! "j" ::my-trigger (fn [] (focus-next-bookmark)))
      (key/bind! "k" ::my-trigger (fn [] (focus-prev-bookmark)))
      (key/bind! "enter" ::my-trigger
                 (fn []
                   (let [focused-bookmark (:focused-bookmark @app-state)]
                     (when focused-bookmark
                       (print focused-bookmark
                        )))))
      (get-bookmarks))
    :reagent-render
    (fn []
      [grid
       ;; Using map-indexed because I need a counter
       ;; to decide when to highlight "@focused-bookmark-idx" item
       (doall (map-indexed bookmark-component (visible-bookmarks)))])}))

(defn search-field []
  (let [search-field! (clojure.core/atom nil)]
    (reagent/create-class
     {:component-did-mount
      ;; FIXME It captures also the "/" character
      ;; FIXME It should check if the search-field is already triggered
      ;; FIXME Esc should unfocus (and empty the search field?)
      (fn [] (key/bind! "/" ::my-trigger
                        (fn []
                          (.focus (first (gdom/getChildren @search-field!))))))
      :reagent-render (fn []
                        [input-base
                         {:ref (fn [el] (reset! search-field! el))
                          :placeholder "Search..."
                          :class "inputInput"
                          :inputProps {:aria-label "search"}
                          :on-change (fn [evt] (swap! app-state assoc :search-filter (event-value evt)))}])})))

(defn appbar []
  [:div {:class "root"}
   [app-bar {:position "static"}
    [toolbar {:variant "dense"}
     [icon-button {:edge "start"
                   :class "menuButton"
                   :color "inherit"
                   :aria-label "open-drawer"}
      [more-vert]]
     [typography {:variant "h6"} "Bookmarks"]
     [typography (clojure.string/join ", " (:tag-filter @app-state))]
     [:div {:class "search"}
      [:div {:class "searchIcon"}
       [search]]
      [search-field]]]]])

(defn main []
  [:<>
   [css-baseline]
   [styles/theme-provider (styles/create-mui-theme custom-theme)
    [:<>
     (appbar)
     [container {:spacing 2 :padding 2}
      [grid {:container true :spacing 2}
       [grid {:item true :xs 8}
        [paper {:elevation 3} [bookmarks-list]]]
       [grid {:item true :xs 4}
        [paper {:elevation 3} [tags-list]]]]]]]])

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [main] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
