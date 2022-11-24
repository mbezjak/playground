(ns core)

;; https://github.com/clj-pdf/clj-pdf
(use 'clj-pdf.core)

(pdf
 [{}
  [:list {:roman true}
   [:chunk {:style :bold} "a bold item"]
   "another item"
   "yet another item"]
  [:phrase "some text"]
  [:phrase "some more text"]
  [:paragraph "yet more text"]]
 "/tmp/doc.pdf")

(def doc1 (java.io.ByteArrayOutputStream.))
(def doc2 (java.io.ByteArrayOutputStream.))
(def doc3 (java.io.ByteArrayOutputStream.))
(pdf [{} "first document"] doc1)
(pdf [{} "second document"] doc2)
(pdf [{} "third document"] doc2)
(with-open [w (clojure.java.io/output-stream "/tmp/doc1.pdf")]
  (.write w (.toByteArray doc1)))

(collate (java.io.FileOutputStream. (clojure.java.io/file "/tmp/merged.pdf"))
         (.toByteArray doc1)
         (.toByteArray doc2)
         (.toByteArray doc3))

;;all keys in the options map are optional
(collate {:title "collated documents"
          :author "john doe"
          :creator "jane doe"
          :orientation :landscape
          :size :a4
          :subject "some subject"}
         (java.io.FileOutputStream. (clojure.java.io/file "/tmp/merged.pdf"))
         (.toByteArray doc1)
         (.toByteArray doc2)
         (.toByteArray doc3))

(pdf
 [{}
  (for [i (range 3)]
    [:paragraph (str "Item: " i)])]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:pdf-table
   [10 20 15]
   ["foo" [:chunk {:style :bold} "bar"] [:phrase "baz"]]
   [[:pdf-cell "foo"] [:pdf-cell "foo"] [:pdf-cell "foo"]]
   [[:pdf-cell "foo"] [:pdf-cell "foo"] [:pdf-cell "foo"]]]]
 "/tmp/doc.pdf")

(def employees
  [{:country "germany",
    :place "nuremberg",
    :occupation "engineer",
    :name "neil chetty"}
   {:country "germany",
    :place "ulm",
    :occupation "engineer",
    :name "vera ellison"}])
(def employee-template
  (template
    [:paragraph
     [:heading $name]
     [:chunk {:style :bold} "occupation: "] $occupation "\n"
     [:chunk {:style :bold} "place: "] $place "\n"
     [:chunk {:style :bold} "country: "] $country
     [:spacer]]))
(employee-template employees)
(pdf
 [{}
  (employee-template employees)]
 "/tmp/doc.pdf")

(def stylesheet
  {:foo {:color [255 0 0]
         :family :helvetica}
   :bar {:color [0 0 255]
         :family :helvetica}
   :baz {:align :right}})
(pdf
 [{:stylesheet stylesheet}
  [:paragraph.foo "item: 0"]
  [:paragraph.bar "item: 1"]
  [:paragraph.bar.baz "item: 2"]]
 "/tmp/doc.pdf")

(pdf
 [{:title "test doc"
   :left-margin 50
   :subject "some subject"
   ;:orientation :landscape
   :font {:size 15}
   :doc-header ["inspired by" "someone else"]
   :watermark {:render (fn [g2d] (.drawString g2d "draft copy" 0 0))
               :translate [100 200]
               :rotate 45
               :scale 8}
   :header "header appears on each page"
   :pages true}
  "contents"]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:chart
   {:type "bar-chart"
    :title "Bar Chart"
    :background [10 100 40]
    :x-label "Items"
    :y-label "Quality"}
   [2 "Foo"] [4 "Bar"] [10 "Baz"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:anchor {:target "http://google.com"} "google"]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:paragraph
   [:anchor {:style {:size 15} :leading 20 :id "targetanchor"} "some anchor"]]
  [:paragraph
   [:anchor {:target "#targetanchor"} "this anchor points to some anchor"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:chapter "first chapter"]
  [:chunk {:style :bold} "small chunk of text"]
  [:chunk {:styles [:bold :italic]} "small chunk of text"]
  [:chunk {:background [0 255 0]} "green chunk"]
  [:chunk {:color [0 0 0] :background [255 0 0]} "more fun with color"]
  [:chunk {:super true} "5"]
  [:chunk {:sub true} "2"]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [[:paragraph "this is on page 1"] [:clear-double-page] [:paragraph "this is on page 3"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [[:paragraph "this is on page 1"]
   [:clear-double-page] [:clear-double-page]
   [:paragraph "this is on page 3"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [[:paragraph "this is on page 1"] [:pagebreak]
   [:paragraph "this is on page 2"] [:clear-double-page]
   [:paragraph "this is on page 3"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [[:paragraph "this is on page 1"] [:pagebreak]
   [:paragraph "this is on page 2"] [:pagebreak]
   [:paragraph "this is on page 3"] [:clear-double-page]
   [:paragraph "this is on page 5"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  ;; :clear-double-page on an empty page 1 does nothing
  [[:clear-double-page] [:paragraph "this is on page 1"]]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:graphics {:under true :translate [100 100]}
   (fn [g2d]
     (doto g2d
       (.setColor java.awt.Color/RED)
       (.drawOval (int 0) (int 0) (int 50) (int 50))
                                          ; Requires :register-system-fonts? true & font availability
       (.setFont (java.awt.Font. "GillSans-SemiBold" java.awt.Font/PLAIN 12))
       (.drawString "A red circle." (float -5) (float 64))))]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:line]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:multi-column
   {:columns 3 :height 100}
   "this text will be split into three columns"]]
 "/tmp/doc.pdf")

(pdf
 [{}
  [:paragraph "first"]
  [:spacer 5]
  [:paragraph "second"]]
 "/tmp/doc.pdf")


(defn radians [degrees] (Math/toRadians degrees))

(defmacro rot [g2d angle & body]
  `(do (. ~g2d rotate (radians ~angle))
    (do ~@body)
    (. ~g2d rotate (radians (- 0 ~angle)))))

(defmacro trans [g2d dx dy & body]
  `(do (. ~g2d translate ~dx ~dy)
    (do ~@body)
    (. ~g2d translate (- 0 ~dx) (- 0 ~dy))))

(import [java.awt Color])

(defn draw-tree [g2d length depth]
  (when (pos? depth)
    (.drawLine g2d 0 0 length 0)
    (trans g2d (int length) 0
      (rot g2d -30 (draw-tree g2d (* length 0.75) (- depth 1)))
      (rot g2d 30 (draw-tree g2d (* length 0.75) (- depth 1))))))

(pdf
  [{:title         "Test doc"
    :header        "page header"
    :subject       "Some subject"
    :creator       "Jane Doe"
    :doc-header    ["inspired by" "William Shakespeare"]
    :right-margin  50
    :author        "John Doe"
    :bottom-margin 10
    :left-margin   10
    :top-margin    20
    :size          "a4"
    :footer        "page"}

   [:heading "Lorem Ipsum"]

   [:paragraph
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec non iaculis lectus. Integer vel libero libero. Phasellus metus augue, consequat a viverra vel, fermentum convallis sem. Etiam venenatis laoreet quam, et adipiscing mi lobortis sit amet. Fusce eu velit vitae dolor vulputate imperdiet. Suspendisse dui risus, mollis ut tempor sed, dapibus a leo. Aenean nisi augue, placerat a cursus eu, convallis viverra urna. Nunc iaculis pharetra pretium. Suspendisse sit amet erat nisl, quis lacinia dolor. Integer mollis placerat metus in adipiscing. Fusce tincidunt sapien in quam vehicula tincidunt. Integer id ligula ante, interdum sodales enim. Suspendisse quis erat ut augue porta laoreet."]

   [:paragraph
    "Sed pellentesque lacus vel sapien facilisis vehicula. Quisque non lectus lacus, at varius nibh. Integer porttitor porttitor gravida. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus accumsan ante tincidunt magna dictum vulputate. Maecenas suscipit commodo leo sed mattis. Morbi dictum molestie justo eu egestas. Praesent lacus est, euismod vitae consequat non, accumsan in justo. Nam rhoncus dapibus nunc vel dignissim."]

   [:paragraph
    "Nulla id neque ac felis tempor pretium adipiscing ac tortor. Aenean ac metus sapien, at laoreet quam. Vivamus id dui eget neque mattis accumsan. Aliquam aliquam lacinia lorem ut dapibus. Fusce aliquam augue non libero viverra ut porta nisl mollis. Mauris in justo in nibh fermentum dapibus at ut erat. Maecenas vitae fermentum lectus. Nunc dolor nisl, commodo a pellentesque non, tincidunt id dolor. Nulla tellus neque, consectetur in scelerisque vitae, cursus vel urna. Phasellus ullamcorper ultrices nisi ac feugiat."]

   [:table {:header [{:background-color [100 100 100]} "FOO"] :cell-spacing 20}
    ["foo"
     [:cell
      [:phrase
       {:style "italic" :size 18 :family "helvetica" :color [200 55 221]}
       "Hello Clojure!"]]
     "baz"]
    ["foo1" [:cell {:background-color [100 10 200]} "bar1"] "baz1"]
    ["foo2" "bar2" [:cell [:table ["Inner table Col1" "Inner table Col2" "Inner table Col3"]]]]]

   [:paragraph
    "Suspendisse consequat, mauris vel feugiat suscipit, turpis metus semper metus, et vulputate sem nisi a dolor. Duis egestas luctus elit eget dignissim. Vivamus elit elit, blandit id volutpat semper, luctus id eros. Duis scelerisque aliquam lorem, sed venenatis leo molestie ac. Vivamus diam arcu, sodales at molestie nec, pulvinar posuere est. Morbi a velit ante. Nulla odio leo, volutpat vitae eleifend nec, luctus ac risus. In hac habitasse platea dictumst. In posuere ultricies nulla, eu interdum erat rhoncus ac. Vivamus rutrum porta interdum. Nulla pulvinar dui quis velit varius tristique dignissim sem luctus. Aliquam ac velit enim. Sed sed nisi faucibus ipsum congue lacinia. Morbi id mi in lectus vehicula dictum vel sed metus. Sed commodo lorem non nisl vulputate elementum. Fusce nibh dui, auctor a rhoncus eu, rhoncus eu eros."]

   [:paragraph
    "Nulla pretium ornare nisi at pulvinar. Praesent lorem diam, pulvinar nec scelerisque et, mattis vitae felis. Integer eu justo sem, non molestie nisl. Aenean interdum erat non nulla commodo pretium. Quisque egestas ullamcorper lacus id interdum. Ut scelerisque, odio ac mollis suscipit, libero turpis tempus nulla, placerat pretium tellus purus eu nunc. Donec nec nisi non sem vehicula posuere et eget sem. Aliquam pretium est eget lorem lacinia in commodo nisl laoreet. Curabitur porttitor dignissim eros, nec semper neque tempor non. Duis elit neque, sagittis vestibulum consequat ut, rhoncus sed dui."]

   [:anchor {:style {:size 15} :leading 20} "some anchor"]

   [:anchor [:phrase {:style "bold"} "some anchor phrase"]]

   [:anchor "plain anchor"]

   [:chunk {:style "bold"} "small chunk of text"]

   [:phrase "some text here"]

   [:phrase {:style "italic" :size 18 :family "helvetica" :color [0 255 221]} "Hello Clojure!"]

   [:chapter [:paragraph "Second Chapter"]]

   [:paragraph {:keep-together true :indent 20} "a fine paragraph"]

   [:list {:roman true} [:chunk {:style "bold"} "a bold item"] "another item" "yet another item"]

   [:chapter "Charts"]

   [:chart
    {:type :bar-chart :title "Bar Chart" :x-label "Items" :y-label "Quality"}
    [2 "Foo"]
    [4 "Bar"]
    [10 "Baz"]]

   [:chart
    {:type :line-chart :title "Line Chart" :x-label "checkpoints" :y-label "units"}
    ["Foo" [1 10] [2 13] [3 120] [4 455] [5 300] [6 600]]
    ["Bar" [1 13] [2 33] [3 320] [4 155] [5 200] [6 300]]]

   [:chart {:type :pie-chart :title "Big Pie"} ["One" 21] ["Two" 23] ["Three" 345]]

   [:chart
    {:type :line-chart
     :time-series true
     :title "Time Chart"
     :x-label "time"
     :y-label "progress"}
    ["Incidents"
     ["2011-01-03-11:20:11" 200]
     ["2011-01-03-11:25:11" 400]
     ["2011-01-03-11:35:11" 350]
     ["2011-01-03-12:20:11" 600]]]

   [:chapter "Graphics2D"]

   [:paragraph
    "Tree Attribution: "
    [:anchor
     {:style {:color [0 0 200]}
      :target "http://www.curiousattemptbunny.com/2009/01/simple-clojure-graphics-api.html"}
     "http://www.curiousattemptbunny.com/2009/01/simple-clojure-graphics-api.html"]]

   [:graphics {:under false :translate [53 120]}
    (fn [g2d]
      (doto g2d
        (.setColor Color/BLACK)
        (.setFont  (java.awt.Font. "SansSerif" java.awt.Font/BOLD 20))
        (.drawString ":graphics Drawing" (float 0) (float 0))))]

   [:graphics {:translate [150 300] :rotate (radians -90)}
     (fn [g2d]
       (.setColor g2d Color/GREEN)
       (draw-tree g2d 50 10))]

   [:graphics {:under false :translate [70 270] :rotate (radians -35)}
    (fn [g2d]
      (doto g2d
        (.setColor (Color. 96 96 96))
        (.setFont  (java.awt.Font. "Serif" java.awt.Font/PLAIN 14))
        (.drawString "drawString with setFont and rotate" (float 0) (float 0))))]

   [:chart {:type :pie-chart
            :title "Vector Pie"
            :vector true
            :width 300 :height 250
            :translate [270 100]}
    ["One" 21] ["Two" 23] ["Three" 345]]

   [:chart
    {:type :line-chart
     :title "Vector Line Chart"
     :x-label "checkpoints"
     :y-label "units"
     :vector true
     :width 500 :height 300
     :translate [50 400]}
    ["Foo" [1 10] [2 13] [3 120] [4 455] [5 300] [6 600]]
    ["Bar" [1 13] [2 33] [3 320] [4 155] [5 200] [6 300]]]

   [:chapter "Embedded SVG"]

   [:paragraph
    "Attribution: "
    [:anchor
     {:style {:color [0 0 200]}
      :target "https://en.wikipedia.org/wiki/File:Example.svg"}
     "https://en.wikipedia.org/wiki/File:Example.svg"]]

   [:pagebreak]

   [:paragraph
    "Attribution: "
    [:anchor
     {:style {:color [0 0 200]}
      :target "https://commons.wikimedia.org/wiki/SVG_examples"}
     "https://commons.wikimedia.org/wiki/SVG_examples"]]

   [:svg {}
     "<?xml version=\"1.0\"?>
      <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
      <svg xmlns=\"http://www.w3.org/2000/svg\" width=\"467\" height=\"462\">
        <rect x=\"80\" y=\"60\" width=\"250\" height=\"250\" rx=\"20\" style=\"fill:#ff0000; stroke:#000000;stroke-width:2px;\" />
        <rect x=\"140\" y=\"120\" width=\"250\" height=\"250\" rx=\"40\" style=\"fill:#0000ff; stroke:#000000; stroke-width:2px; fill-opacity:0.7;\" />
      </svg>"]

   [:svg {:translate [100 450]}
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
      <!DOCTYPE svg>
      <svg xmlns=\"http://www.w3.org/2000/svg\" width=\"304\" height=\"290\">
         <path d=\"M2,111 h300 l-242.7,176.3 92.7,-285.3 92.7,285.3 z\"
               style=\"fill:#FB2;stroke:#F00;stroke-width:3;stroke-linejoin:round\"/>
      </svg>"]]

  "/tmp/example.pdf")
