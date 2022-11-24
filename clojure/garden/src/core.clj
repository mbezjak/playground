(ns core)

;; https://github.com/noprompt/garden
(require '[garden.core :refer [css]])

(css [:body {:font-size "16px"}])
(css [:.header {:font-size "16px"}])
(css [:.a :.b {:font-size "16px"}])
(css [:.error [:ul {:font-size "16px"}]])
(css [:a {:font-weight 'bold}])

(css [:.dvp-tree-node-disabled [:.x-grid-cell {:-moz-opacity 0.5
                                               :opacity 0.5
                                               :filter "alpha(opacity=50)"}]])
(css [:.data-search-loading-icon
      {:background-image "url(../images/ajax-loader.gif)"
       :background-repeat 'no-repeat
       :background-position "center !important"
       :background-color "#E9EBF9"
       :cursor 'wait}])
(css [:.data-search-loading-icon
      {:background {:image "url(../images/ajax-loader.gif)"
                    :repeat 'no-repeat
                    :position "center !important"
                    :color "#E9EBF9"}
       :cursor 'wait}])
(css {:vendors ["moz" "webkit"]
      :auto-prefix #{:cursor}}
     [:.data-search-loading-icon
      {:background {:image "url(../images/ajax-loader.gif)"
                    :repeat 'no-repeat
                    :position "center !important"
                    :color "#E9EBF9"}
       :cursor 'wait}])
