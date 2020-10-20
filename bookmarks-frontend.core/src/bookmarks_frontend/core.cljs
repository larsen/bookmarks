(ns ^:figwheel-hooks bookmarks-frontend.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [cljs.core]
   [clojure.set]
   [clojure.string]
   [goog.dom :as gdom]
   [reagent-material-ui.components :as mui]
   [reagent-material-ui.colors :as colors]
   [reagent-material-ui.icons.more-vert :refer [more-vert]]
   [reagent-material-ui.icons.search :refer [search]]
   [reagent-material-ui.styles :as styles]
   [reagent-material-ui.core.fade :refer [fade]]

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
            :backgroundColor "white"  ;; (fade colors/green 0.15)
            :marginLeft 0
            :width "100%"
            }
   :search-icon {:padding (spacing 2)
                :height "100%"
                :position "absolute"
                :pointerEvents "none"
                :display "flex"
                :alignItems "center"
                :justifyContent "center"}
   :button     {:margin (spacing 1)}})

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
    [mui/chip {:key (gensym "tag-")
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
                                           [mui/grid
                                            (doall (for [tag (:tags @app-state)]
                                                     (tag-component tag :with-count true)))])}))

(defn bookmark-component [counter bookmark]
  [mui/card {:key (gensym "bookmark-")
         :variant "outlined"}
   [mui/card-header {:title (:description bookmark)}]
   [mui/card-content
    [mui/typography {:variant "body2"}
     [:a {:href (:url bookmark)} (short-printable-url (:url bookmark))]]
    (when (= counter @focused-bookmark-idx)
      [mui/typography {:variant "caption"} "current"])]
   [mui/card-actions
    [mui/button "Link"]
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
      [mui/grid
       ;; Using map-indexed because I need a counter
       ;; to decide when to highlight "@focused-bookmark-idx" item
       (doall (map-indexed bookmark-component (visible-bookmarks)))])}))

(defn search-field [{:keys [classes] :as props}]
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
                        ;; FIXME with this structure it breaks the keybind
                        [:div {:class (:search classes)}
                         [:div {:class (:search-icon classes)}
                          [search]]
                         [mui/input-base
                          {:ref (fn [el] (reset! search-field! el))
                           :placeholder "Search..."
                           :class (:search classes)
                           :inputProps {:aria-label "search"}
                           :on-change (fn [evt] (swap! app-state assoc :search-filter (event-value evt)))}]])})))

(defn appbar []
  [:div {:class "root"}
   [mui/app-bar {:position "static"}
    [mui/toolbar {:variant "dense"}
     [mui/icon-button {:edge "start"
                   :class "menuButton"
                   :color "inherit"
                   :aria-label "open-drawer"}
      [more-vert]]
     [mui/typography {:variant "h6"} "Bookmarks"]
     [mui/typography (clojure.string/join ", " (:tag-filter @app-state))]
     [(with-custom-styles search-field)]]]])

(defn main []
  [:<>
   [mui/css-baseline]
   [styles/theme-provider (styles/create-mui-theme custom-theme)
    [:<>
     (appbar)
     [mui/container {:spacing 2 :padding 2}
      [mui/grid {:container true :spacing 2}
       [mui/grid {:item true :xs 8}
        [mui/paper {:elevation 3} [bookmarks-list]]]
       [mui/grid {:item true :xs 4}
        [mui/paper {:elevation 3} [tags-list]]]]]]]])

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
