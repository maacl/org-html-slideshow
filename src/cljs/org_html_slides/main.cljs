(ns org-html-slides.main
  (:require [goog.debug.Logger :as Logger]
            [goog.debug.Console :as Console]
            [goog.array :as array]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.style :as style]
            [goog.events :as events]
            [goog.events.KeyHandler :as KeyHandler]
            [goog.events.KeyCodes :as KeyCodes]
            [goog.Uri :as Uri]))

(def logger (Logger/getLogger "org_html_slides.main"))

(defn info [msg]
  (.info logger msg))

(def slideshow-mode (atom false))

;;; body

(defn get-body []
  (first (array/toArray (dom/getElementsByTagNameAndClass "body"))))

(defn set-body [elem]
  (let [body (first (array/toArray (dom/getElementsByTagNameAndClass "body")))
        parent (. body parentNode)]
    (.removeChild parent body)
    (.appendChild parent (.cloneNode elem true))))

(def document-body (.cloneNode (get-body) true))

;;; location fragment

(defn location-fragment []
  (let [uri (Uri/parse (. js/window location))]
    (when (. uri (hasFragment))
      (. uri (getFragment)))))

(defn set-location-fragment [id]
  (set! (. js/window location)
        (. (Uri/parse (. js/window location)) (setFragment id))))

(defn current-element []
  (if-let [id (location-fragment)]
    (dom/getElement id document-body)
    (. (dom/getDocument) firstChild)))

;;; stylesheets

(defn stylesheet-link-elems [media-type]
  (vec (filter (fn [elem]
                 (and (= "stylesheet" (.. elem (getAttribute "rel") (toLowerCase)))
                      (. elem (getAttribute "media"))
                      (= media-type (.. elem (getAttribute "media") (toLowerCase)))))
               (array/toArray (dom/getElementsByTagNameAndClass "link")))))

(def original-screen-stylesheet-links
  (stylesheet-link-elems "screen"))

(def original-projection-stylesheet-links
  (stylesheet-link-elems "projection"))

(defn containing-slide-div [marker-elem]
  (some identity (for [n (range 8 0 -1)]
                   (dom/getAncestorByTagNameAndClass
                    marker-elem "div" (str "outline-" n)))))

(defn all-slide-markers []
  (array/toArray (dom/getElementsByTagNameAndClass "span" "slide")))

(defn all-slides []
  (vec (map containing-slide-div (all-slide-markers))))

(defn node-seq
  "Depth-first walk of the DOM as a lazy sequence, starting at elem."
  [elem]
  (when elem
   (lazy-seq
    (cons elem (node-seq
                (or (. elem firstChild)
                    (. elem nextSibling)
                    (when-let [parent (. elem parentNode)]
                      (. parent nextSibling))))))))

(defn first-slide-marker-after [elem]
  (first (filter (fn [elem]
                    (info (str "Checking element " elem))
                   (and (= "SPAN" (. elem nodeName))
                        (classes/has elem "slide")))
                 (node-seq elem))))

(defn second-slide-marker-after [elem]
  (second (filter (fn [elem]
                    (info (str "Checking element " elem))
                   (and (= "SPAN" (. elem nodeName))
                        (classes/has elem "slide")))
                 (node-seq elem))))

(defn add-to-head [elem]
  (.appendChild (first (array/toArray (dom/getElementsByTagNameAndClass "head")))
                elem))

(defn remove-elem [elem]
  (.. elem parentNode (removeChild elem)))

;;; show

(defn show-original-html []
  (set-body document-body))

(defn show-current-slide []
  (info "Showing slide")
  (let [slide (containing-slide-div (first-slide-marker-after (current-element)))]
    (info (str "Next slide found: " slide))
    (set-body (dom/createDom "body" nil slide))
    (set-location-fragment (. slide id))))

;;; slideshow mode

(defn enter-slideshow-mode []
  (info "Entering slideshow mode")
  (show-current-slide)
  (doseq [elem (stylesheet-link-elems "screen")]
    (remove-elem elem))
  (doseq [elem original-projection-stylesheet-links]
    (.setAttribute elem "media" "screen")
    (add-to-head elem)))

(defn leave-slideshow-mode []
  (info "Leaving slideshow mode")
  (show-original-html)
  (doseq [elem (stylesheet-link-elems "screen")]
    (remove-elem elem))
  (doseq [elem original-screen-stylesheet-links]
    (add-to-head elem))
  (. (current-element) (scrollIntoView)))

(defn show-next-slide []
  (set-location-fragment (. (containing-slide-div (second-slide-marker-after (current-element))) id))
  (show-current-slide))

(defn show-prev-slide []
  (when (pos? @current-slide)
    (swap! current-slide dec)
    (show-current-slide)))

(defn toggle-mode []
  (if @slideshow-mode
    (leave-slideshow-mode)
    (enter-slideshow-mode))
  (swap! slideshow-mode not))

(defn handle-key [event]
  (let [code (. event keyCode)]
    (if @slideshow-mode
      (condp = code
        goog.events.KeyCodes.T (toggle-mode)

        goog.events.KeyCodes.SPACE (show-next-slide)
        goog.events.KeyCodes.ENTER (show-next-slide)        
        goog.events.KeyCodes.MAC_ENTER (show-next-slide)
        goog.events.KeyCodes.RIGHT (show-next-slide)
        goog.events.KeyCodes.DOWN (show-next-slide)
        goog.events.KeyCodes.PAGE_DOWN (show-next-slide)
        goog.events.KeyCodes.N (show-next-slide)

        goog.events.KeyCodes.LEFT (show-prev-slide)
        goog.events.KeyCodes.UP (show-prev-slide)
        goog.events.KeyCodes.PAGE_UP (show-prev-slide)
        goog.events.KeyCodes.P (show-prev-slide)
        nil)
      (condp = code
        goog.events.KeyCodes.T (toggle-mode)
        nil))))

(defn install-keyhandler []
  (events/listen (goog.events.KeyHandler. (dom/getDocument))
                 goog.events.KeyHandler.EventType.KEY
                 handle-key))

(defn add-image-classes []
  (doseq [img (array/toArray (dom/getElementsByTagNameAndClass "img"))]
    (let [p (. img parentNode)]
      (when (= "P" (. p nodeName))
        (classes/add p "image")))))

(defn main []
  (.setCapturing (goog.debug.Console.) true)
  (info "Application started")
  (info (str "Found " (count original-screen-stylesheet-links) " screen stylesheets."))
  (info (str "Found " (count original-projection-stylesheet-links) " projection stylesheets."))
  (add-image-classes)
  (install-keyhandler))

(main)
