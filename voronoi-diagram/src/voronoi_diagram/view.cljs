(ns voronoi-diagram.view
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as vsm]
            [goog.math.Size :as size]
            [goog.graphics.CanvasGraphics :as canvas]
            [goog.graphics :as graphics]
            [clojure.browser.event :as event]
            [goog.graphics.Stroke :as stroke]
            [clojure.browser.repl :as repl]
            [goog.graphics.SolidFill :as fill]))

(def canvas
  (doto (graphics/createGraphics. "100%" "100%")
    (.render (dom/getElement "drawing"))))

(def pts (atom []))

(defn get-size []
  (dom/getViewportSize))

(defn size-str []
  (let [size-obj (get-size)]
    (. size-obj (toString))))

(defn size-vec [size]
  (vector (. size width) (. size height)))

(defn do-voronoi [e]
  (.log js/console "do voronoi"))

(defn display-find-voronoi-button []
  (.log js/console "displaying voronoi button")
  (let [[width height] (size-vec (get-size))
        font (graphics/Font. 20 "Helvetica")
        text "Calculate Voronoi"
        text-width 160
        stroke (graphics/Stroke. 1 "black")
        button-rec (.drawRect canvas (- (/ width 2) (/ text-width 2) 10) (- height 20) (+ text-width 20) 20 stroke (graphics/SolidFill. "white" 0.0))
        text-element (.drawTextOnLine canvas text (- (/ width 2) (/ text-width 2)) (- height 10) (+ (/ width 2) (/ text-width 2)) (- height 10) "left" font stroke (graphics/SolidFill. "black" 1.0))]
    (event/listen button-rec :click do-voronoi)
    (event/listen text-element :click do-voronoi)))

(defn draw-pt [e]
  (.log js/console (. e target))
  (when (= (. e target) (. canvas (getElement)))
   (let [pt-x (. e offsetX)
         pt-y (. e offsetY)]
     (.log js/console "pt found")
     (swap! pts conj [pt-x pt-y])
     (if (= (count @pts) 2)
       (display-find-voronoi-button))
     (. canvas (drawEllipse pt-x pt-y 3 3 (graphics/Stroke. 1 "black") (graphics/SolidFill. "red" 1.0))))))


(defn set-up-canvas [[width height]]
  (let [svg-element (. canvas (getElement))
        _ (event/listen svg-element :click draw-pt)]))

(set-up-canvas (size-vec (get-size)))

;(repl/connect "http://localhost:9000/repl")

