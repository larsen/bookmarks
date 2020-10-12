(ns ^:figwheel-hooks bookmarks-frontend.client
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   )
   )

(defn get-bookmarks []
  (go (let [response (<! (http/get "http://localhost:5000/bookmarks"
                                   {:with-credentials? false}))]
        (:bookmarks (:body response)))))

(defn get-tags []
  (go (let [response (<! (http/get "http://localhost:5000/tags"
                                   {:with-credentials? false}))]
        (:tags (:body response)))))
