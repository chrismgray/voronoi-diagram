(ns visibility-polygon.view
  (:require [voronoi-diagram.pt :as pt]
            [voronoi-diagram.core :as core]
            [voronoi-diagram.seg :as seg]
            [voronoi-diagram.rect :as rect])
  (:import [java.awt.event MouseEvent])
  (:use [rosado.processing :as proc]
        [rosado.processing.applet :as app]))

(def pts (atom []))
(def v-d-rect (atom {}))
(def mouse-position (atom (pt/new-pt 0 0)))

(defn draw []
  (proc/background-float 125)
  (proc/stroke-weight 4)
  (proc/stroke-float 10)
  (when (seq @v-d-rect)
    (doseq [region (vals (:regions @v-d-rect))]
      (proc/begin-shape)
;      (proc/fill-float 255 0)
      (doseq [seg region]
        (proc/vertex (get-in seg [:e1 :x]) (get-in seg [:e1 :y])))
      (proc/end-shape :close)))
  (doseq [pt @pts]
    (proc/stroke-weight 4)
    (proc/stroke-float 10)
    (proc/point (pt :x) (pt :y))
    (proc/stroke-weight 1)
    (proc/stroke-float 0)
    (proc/string->text (str "(" (pt :x) ", " (pt :y) ")") (pt :x) (pt :y)))
  (let [last-pt @mouse-position]
    (proc/stroke-weight 5)
    (proc/stroke-float 155)
    (proc/point (last-pt :x) (last-pt :y))))


(defn setup []
  (proc/smooth)
  (proc/text-font (proc/create-font "Helvetica" 10 true))
  (proc/no-fill))

(defn mouse-moved [evt]
  (let [new-mouse-pos (pt/new-pt (.getX evt) (.getY evt))]
    (reset! mouse-position new-mouse-pos)))

(defn mouse-clicked [evt]
  (let [pt (pt/new-pt (.getX evt) (.getY evt))]
    (when (= (.getID evt) MouseEvent/MOUSE_CLICKED)
     (swap! pts conj pt))))

(app/defapplet visibility-polygon-example :title "Visibility Polygon"
  :size [500 500]
  :setup setup :draw draw
  :mouse-moved mouse-moved
  :mouse-clicked mouse-clicked)

(comment
  (reset! pts [])
  @pts
  @mouse-position
  (reset! mouse-position {:x 292 :y 292})
  (reset! mouse-position {:x 322 :y 303})
  (reset! mouse-position {:x 411 :y 228})

  (reset! v-d-rect (apply core/voronoi-diagram (rect/new-rect (pt/new-pt 0 500) (pt/new-pt 500 0)) @pts))
  (reset! pts (rest @pts))
  (second (vals (:regions @v-d-rect)))
  (reset! v-d-rect {})
  
  (def visibility-polygon-example nil)

  (app/run visibility-polygon-example)
  (app/stop visibility-polygon-example))