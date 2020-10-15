(ns ^:figwheel-hooks bookmarks-frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [bookmarks-frontend.components]
   [bookmarks-frontend.client]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent-material-ui.core.css-baseline :refer [css-baseline]]
   [reagent-material-ui.core.container :refer [container]]
   [reagent-material-ui.core.grid :refer [grid]]
   [reagent-material-ui.core.app-bar :refer [app-bar]]
   [reagent-material-ui.core.paper :refer [paper]]
   [reagent-material-ui.core.toolbar :refer [toolbar]]
   [reagent-material-ui.core.typography :refer [typography]]
   [reagent-material-ui.core.input-base :refer [input-base]]
   [reagent-material-ui.core.chip :refer [chip]]
   [reagent-material-ui.colors :as colors]
   [reagent-material-ui.styles :as styles]
   [cljs.core]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]))

(defonce app-state
  (atom {:bookmarks []
         :tags []
         :search-filter ""}))

(defn get-bookmarks []
  (go (let [response (<! (http/get "http://localhost:5000/bookmarks"
                                   {:with-credentials? false}))]
        (swap! app-state assoc :bookmarks (:body response)))))

(defn get-tags []
  (go (let [response (<! (http/get "http://localhost:5000/tags"
                                   {:with-credentials? false}))]
        (swap! app-state assoc :tags (:body response)))))

(def custom-theme
  {:spacing 2
   :palette {:primary   colors/orange
             :secondary colors/green}})

(defn custom-styles [{:keys [spacing] :as theme}]
  {:button     {:margin (spacing 1)}
   :text-field {:width        200
                :margin-left  (spacing 1)
                :margin-right (spacing 1)}})

(def with-custom-styles (styles/with-styles custom-styles))

(defn event-value
  [^js/Event e]
  (.. e -target -value))

(defn bookmark-matches-filter [bookmark search-filter]
  (if (not-empty search-filter)
    (let [filter-re (re-pattern search-filter)]
      (or (not-empty (cljs.core/re-find filter-re (:description bookmark)))
          (not-empty (cljs.core/re-find filter-re (:url bookmark)))))
    true))

(defn bookmark-component [bookmark]
  [grid
   [paper {:elevation 3 :spacing 2}
    [typography {:variant "body1"}
     [:a {:href (:url bookmark)} (:description bookmark)]]
    [typography {:variant "body2"}
     (:url bookmark)]
    (for [tag (:tags bookmark)]
      [chip {:size "small" :label tag}])]])

(defn bookmarks-list []
  [grid
   [:div
    (for [bookmark (filter #(bookmark-matches-filter % (:search-filter @app-state))
                           (:bookmarks @app-state))]
      (bookmark-component bookmark))]])

(defn tags-list []
  [grid
   (for [tag (:tags @app-state)]
     [chip {:size "small" :label (:name tag)}])])

(defn appbar []
  [app-bar {:position "static"}
   [toolbar {:variant "dense"}
    [typography {:variant "h6"} "Bookmarks"]
    [:div
     [input-base {:placeholder "Search..."
                  :on-change (fn [evt]
                               (swap! app-state assoc :search-filter (event-value evt)) )}]]]])

(defn main []
  [:<>
   [css-baseline]
   [styles/theme-provider (styles/create-mui-theme custom-theme)
    [:<>
     (appbar)
     [container {:maxWidth "sm"}
      [grid {:container true}
       [grid {:item true :xs 8}
        [paper {:elevation 3} (bookmarks-list)]]
       [grid {:item true :xs 4}
        [paper {:elevation 3} (tags-list)]]]]]]])

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
(get-bookmarks)
(get-tags)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  (get-bookmarks)
  (get-tags)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

