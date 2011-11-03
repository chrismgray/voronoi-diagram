(ns voronoi-diagram.view
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as vsm]
            [goog.math.Size :as size]
            [goog.graphics.CanvasGraphics :as canvas]
            [goog.graphics :as graphics]
            [clojure.browser.event :as event]
            [goog.graphics.Stroke :as stroke]
            [clojure.browser.repl :as repl]
            [voronoi-diagram.core :as core]
            [voronoi-diagram.pt :as pt]
            [voronoi-diagram.rect :as rect]
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

(defn line-to [path segs]
  (doseq [[x y] (partition 2 segs)]
    (.lineTo path x y)))

(defn draw-region [r]
  (let [{_1 first-pt-x _2 first-pt-y} (get-in r [0 :e1])
        rest-pts (mapcat (fn [{_1 x _2 y}] [x y]) (map :e2 r))
        path (doto (graphics/Path.)
               (.moveTo first-pt-x first-pt-y)
               (line-to rest-pts))]
   (.drawPath canvas path stroke fill)))

(defn do-voronoi [e]
  (.log js/console "do voronoi")
  (let [[width height] (size-vec (get-size))
        voronoi-rect (apply core/diagram (rect/new-rect 0 0 width height) @pts)]
    (doseq [region (voronoi-rect :regions)]
      (draw-region region))))

(defn display-find-voronoi-button []
  (.log js/console "displaying voronoi button")
  (let [[width height] (size-vec (get-size))
        font (graphics/Font. 20 "Helvetica")
        text "Calculate Voronoi"
        text-width 160
        stroke (graphics/Stroke. 1 "black")
        rec-height-offset 40
        text-height-offset 30
        button-rec (.drawRect canvas (- (/ width 2) (/ text-width 2) 10) (- height rec-height-offset) (+ text-width 20) 20 stroke (graphics/SolidFill. "white" 0.0))
        text-element (.drawTextOnLine canvas text (- (/ width 2) (/ text-width 2)) (- height text-height-offset) (+ (/ width 2) (/ text-width 2)) (- height text-height-offset) "left" font stroke (graphics/SolidFill. "black" 1.0))]
    (event/listen button-rec :click do-voronoi)
    (event/listen text-element :click do-voronoi)))

(defn draw-pt [e]
  (.log js/console (. e target))
  (when (= (. e target) (. canvas (getElement)))
   (let [pt-x (. e offsetX)
         pt-y (. e offsetY)]
     (.log js/console "pt found")
     (swap! pts conj (pt/new-pt pt-x pt-y))
     (if (= (count @pts) 2)
       (display-find-voronoi-button))
     (. canvas (drawEllipse pt-x pt-y 3 3 (graphics/Stroke. 1 "black") (graphics/SolidFill. "red" 1.0))))))


(defn set-up-canvas [[width height]]
  (let [svg-element (. canvas (getElement))
        _ (event/listen svg-element :click draw-pt)]))

(set-up-canvas (size-vec (get-size)))


