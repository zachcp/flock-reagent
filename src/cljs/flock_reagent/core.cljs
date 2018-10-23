(ns flock_reagent.core
  (:require
   [reagent.core :as reagent]
   [mondrian.ui :as ui]
   [mondrian.math :as math]
   [monet.canvas :as m]
   [flock-reagent.flockmath :as fm]
   [flock-reagent.config :as config]))


;;---------------------------------------------------------------------
;; App State

(def initial-birds
  (set (for [i (range 0 20)]
            {:x ( + 50 (rand-int 600))
             :y ( + 50 (rand-int 400))
             :direction (rand-int 360)})))

(def app-state
  (reagent/atom
    {:birds initial-birds
     :w               650
     :h               400
     :alignment       0.1
     :seperation      0.2
     :cohesion        0.1
     :seperation-r    50
     :alignment-r     80
     :cohesion-r      150
     :off-the-wall    0.4
     :delta-t-ms      20
     :speed-pps       200
     :persist-image   false
     :show-separation false
     :show-alignment  false
     :show-cohesion   false}))



;;---------------------------------------------------------------------
;; Move Code


(defn move-bird [bird h w delta-pixels]
      (let [
            dir (:direction bird)
            rad (math/radians dir)
            x (+ (:x bird) (math/circle-x delta-pixels rad))
            y (+ (:y bird) (math/circle-y delta-pixels rad))]

           (assoc bird :x x :y y :direction dir)))


(defn mean-alignment [flock]
      (fm/mean-of-angles (map :direction flock)))


(defn update-direction-toward-mean-alignment [bird birds alignment-r rate]
      (let [flock (fm/local-flock bird birds alignment-r)]
           (if (empty? flock)
             bird
             (assoc bird :direction (fm/relative-mean-of-angles (mean-alignment flock) (:direction bird) rate)))))


(defn update-direction-toward-mean-position [bird birds r rate]
      (let [flock (fm/local-flock bird birds r)]
           (if (empty? flock)
             bird
             (assoc bird :direction (fm/relative-mean-of-angles (fm/direction-to bird (fm/mean-poistion flock)) (:direction bird) rate)))))


(defn update-direction-away-mean-position [bird birds r rate]
      (let [flock (fm/local-flock bird birds r)]
           (if (empty? flock)
             bird
             (assoc bird :direction (fm/relative-mean-of-angles (+ 180 (fm/direction-to bird (fm/mean-poistion flock))) (:direction bird) rate)))))


(defn update-direction-off-the-walls [bird w h r rate]
      (let [to (cond
                 (< (:x bird) r) 0
                 (> (:x bird) (- w r)) 180
                 (< (:y bird) r) 90
                 (> (:y bird) (- w r)) 270
                 :else (:direction bird))]

           (assoc bird :direction (fm/relative-mean-of-angles to (:direction bird) rate))))


(defn move-flock
      [{:keys [delta-t-ms speed-pps w h birds seperation-r alignment-r cohesion-r] :as state}]
      ;;  (.log js/console (str  "flock:" birds))
      (let [pixels-per-millisecond (/ speed-pps 1000)
            delta-pixels (* delta-t-ms pixels-per-millisecond)
            new-birds (set (for [b birds]
                                (-> b
                                    (update-direction-toward-mean-alignment birds alignment-r (:alignment state))
                                    (update-direction-toward-mean-position  birds cohesion-r (:cohesion state))
                                    (update-direction-away-mean-position birds seperation-r (:seperation state))
                                    (update-direction-off-the-walls w h seperation-r (:off-the-wall state))
                                    (move-bird h w delta-pixels))))]

           (assoc state :birds new-birds)))



;; ---------------------------------------------------------------------
;; Render Code
;;
(defn clear-background
      [{:keys [ctx w h persist-image]}]
      (when-not persist-image
                (-> ctx
                    (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
                    (m/fill-rect {:x 0 :y 0 :w w :h h}))))


(defn x-y [x y raduis direction]
      (let  [d (math/radians direction)
             dx (math/circle-x raduis d)
             dy (math/circle-y raduis d)]
            [(+ x dx) (+ y dy)]))


(defn draw-bird
      [ctx x y direction]
      ;;  (.log js/console (str  "draw-bird: " x "," y))
      (let [w 5
            h 17
            [x1 y1] (x-y x y w (+ direction 90))
            [x2 y2] (x-y x y h direction)
            [x3 y3] (x-y x y w (+ direction 270))]
           (-> ctx
               (m/fill-style "yellow")
               (m/stroke-style "yellow")
               (m/begin-path)
               (m/move-to x y)
               (m/line-to x1 y1)
               (m/line-to x2 y2)
               (m/line-to x3 y3)
               (m/line-to x y)
               (m/fill)
               (m/stroke)
               (m/close-path))))


(defn empty-circle [ctx {:keys [x y r color]}]
      (-> ctx
          (m/stroke-width 2)
          (m/stroke-style color)
          (m/begin-path)
          (. (arc x y r 0 (* (.-PI js/Math) 2) true)))
      (m/stroke ctx))


(defn draw-circles [ctx x y circles]
      (doseq [c circles]
             (when (:show c)
                   (do
                     (empty-circle ctx { :x x :y y :r (:r c) :color (:color c)})))))



(defn draw-flock [{:keys [ctx w h birds show-seperation seperation-r show-alignment alignment-r show-cohesion cohesion-r persist-image] :as state}]
      (when-not persist-image
                (clear-background state))

      (doseq [b birds]
             (draw-bird ctx (:x b) (:y b) (:direction b))
             (draw-circles ctx (:x b) (:y b)
                           [ {:show show-seperation :r seperation-r :color "blue"}
                            {:show show-alignment :r alignment-r :color "green"}
                            {:show show-cohesion :r cohesion-r :color "gray"}])))


;;---------------------------------------------------------------------
;; Components/Views


(defn slider [atm param min max step]
      [:input {:type "range"
               :value (get @atm param)
               :min min
               :max max
               :step step
               :style {:width "100%"}
               :on-change (fn [e] (swap! atm assoc param (.. e -target -value)))}])


(defn togglebox [atm param]
      [:input.toggle {:type "checkbox"
                      :checked (get @atm param)
                      :on-change (fn [e] (swap! atm update-in [param] not))}])



(defn control-panel []
  [:div
   [:div
    [:div.row
       [:div.col
        [:h5 (str "Seperation: " (get @app-state :seperation) "/" (get @app-state :seperation-r))]
        [slider app-state :seperation 0 1 0.1]
        [slider app-state :seperation-r 0 150 10]
        [:p "Show Seperation: "
         [togglebox app-state :show-seperation]]]

       [:div.col
        [:h5 (str "Cohesion: " (get @app-state :cohesion) "/" (get @app-state :cohesion-r))]
        [slider app-state :cohesion 0 1 0.1]
        [slider app-state :cohesion-r 0 150 10]
        [:p "Show Cohesion: "
         [togglebox app-state :show-cohesion]]]

      [:div.col
       [:h5 (str "Alignment: " (get @app-state :alignment) "/" (get @app-state :alignment-r))]
       [slider app-state :alignment 0 1 0.1]
       [slider app-state :alignment-r 0 150 10]
       [:p "Show Alignment: "
          [togglebox app-state :show-alignment]]]]

    [:hr]

    [:div.row
      [:div.col
       [:h6 "Speed-pps"]
       [slider app-state :speed-pps 0 1000 20]]

      [:div.col
       [:h6 "Delta-t-ms"]
       [slider app-state :delta-t-ms 0 150 10]]

      [:div.col
       [:h6 "Off-The-Wall"]
       [slider app-state :off-the-wall 0 1 0.1]]

      [:div.col
       [:p "persist-Image: "
        [togglebox app-state :persist-image]]]]]])



(defn birds-panel [appstate]
      (reagent/create-class

        {:reagent-render
         (fn [] [:canvas {:width (get appstate :w) :height (get appstate :h)}])

         :component-did-update
         (fn [this old-argv]
             (let [dom-node (reagent/dom-node this)
                   app-state (first (rest (reagent/argv this)))
                   ctx (.getContext dom-node "2d")]
                  ;(clear-canvas ctx 500 300)
                  ;(println app-state)
                  (draw-flock (assoc app-state :ctx ctx))))

         :component-did-mount
         (fn [this]
             (let [ dom-node (reagent/dom-node this)
                   app-state  (first (rest (reagent/argv this)))
                   ctx (.getContext dom-node "2d")]
                  (draw-flock (assoc app-state :ctx ctx))))}))


(defn main-panel []
      [:div.container
       [control-panel]
       [birds-panel @app-state]])


;;---------------------------------------------------------------------
;; Component Setup


;; Set Timer
(js/setInterval  #(swap! app-state move-flock) 10)


(defn dev-setup []
      (when config/debug?
                (enable-console-print!)
                (println "dev mode")))

(defn mount-root []
      (reagent/render [main-panel]
                      (.getElementById js/document "app")))

(defn ^:export init []
      (mount-root))



