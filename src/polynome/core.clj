(ns polynome.core
  (:require  [monome-serial.core :as monome-core]
             [monome-serial.led :as monome]
             [monome-serial.led-at :as monome-at]
             [monome-serial.event-handlers :as handlers])
  (:use [clojure.contrib.ns-utils :only [immigrate]]
        [clojure.contrib.seq-utils :only [find-first]]))

(defrecord Event [time x y action])


(defn button-state
  [m]
  @(get-in m [::core :button-state]))

(defn find-event
  "Returns the first event for which fun returns true or nil if no match is found."
  [bs f]
  (find-first f (:event-history bs)))

(defn prev-event
  [bs x y action]
  (find-event bs #(and (= x (:x %))
                      (= y (:y %))
                      (= action (:action %)))))

(defn prev-press
  [bs x y]
  (prev-event bs x y :press))

(defn prev-release
  [bs x y]
  (prev-event bs x y :release))

(defn on-action
  ([m f] (on-action m f f))
  ([m f name] (swap! (get-in m [::core :callbacks]) conj [name f] )))

(defn on-press
  ([m f] (on-press m f f))
  ([m f name] (on-action m (fn [action x y state]
                             (if (= :press action)
                               (f x y state))))))

(defn on-release
  ([m f] (on-release m f f))
  ([m f name] (on-action m (fn [action x y state]
                             (if (= :release action)
                               (f x y state))))))

(defn on-sustain
  ([m f] (on-sustain m f f))
  ([m f name]
     (on-release m (fn [x y state]
                     (let [press (prev-press state x y)
                           release (prev-release state x y)
                           time (- (:time release) (:time press))]
                       (f x y time state))))))

(defn callbacks
  "Return a list of callbacks associated with monome m"
  [m]
  @(get-in m [::core :callbacks]))


(defn remove-callback
  [m name]
  (println "implement me!"))

(defn remove-all-callbacks
  [m]
  (reset! (get-in m [::core :callbacks]) []))

(defn- detect-type
  [filename]
  (condp re-find filename
    #"-m64-" :64
    #"-m128-" :128l
    #"-m256-" :256
    #"-m512-" :512
    :64))

(defn- infer-range
  [type]
  (case type
        :64 [8 8]
        :128  [16 8]
        :128l [16 8]
        :128p [8 16]
        :256  [16 16]
        :512  [32 16]
        :512l [32 16]
        :512p [16 32]))

(defn init  "Initialise a monome. Raises an exception if the supplied path isn't valid or is already in use"
  ([path] (init path (detect-type path)))
  ([path type] (apply init (cons path (infer-range type))))
  ([path n-cols n-rows]
     (let [m      (monome-core/connect path)
           max-x (dec n-cols)
           max-y (dec n-rows)
           range-x (inc max-x)
           range-y (inc max-y)
           coords (for [y (range range-y)
                        x (range range-x)]
                    [x y])

           history (list)
           led-activation    (into {} (map (fn [el] [el :inactive]) coords))
           button-activation (into {} (map (fn [el] [el :inactive]) coords))
           press-count       (into {} (map (fn [el] [el 0]) coords))

           callbacks (atom [])
           led-state    (atom led-activation)
           button-state (atom {:event-history history
                               :button-activation button-activation
                               :led-activation led-activation
                               :press-count press-count})

           poly-m (assoc m ::core {:max-x max-x
                                   :max-y max-y
                                   :range-x range-x
                                   :range-y range-y
                                   :callbacks callbacks
                                   :coords coords
                                   :button-state button-state})

           update-button-state (fn [state action x y]
                                 (let [event (Event. (System/currentTimeMillis) x y action)
                                       state (update-in state [:event-history] conj event)]
                                   (case action
                                         :press (-> state
                                                    (assoc-in [:button-activation [x y]] :active)
                                                    (update-in [:press-count [x y]] inc))
                                         :release (-> state
                                                      (assoc-in [:button-activation [x y]] :inactive)))))

           update-button-state-handler (fn [action x y]
                                         (let [new-state (swap! button-state update-button-state action x y)]
                                           (doseq [[_ callback] @callbacks] (callback action x y new-state))))]

       (handlers/on-action poly-m update-button-state-handler ::state "update monome state")
       poly-m)))


(defn max-x
  "Returns the monome's maximum x coord"
  [m]
  (get-in m [::core :max-x]))

(defn max-y
  "Returns the monome's maximum y coord"
  [m]
  (get-in m [::core :max-y]))

(defn range-x
  "Returns the number of buttons on the x axis"
  [m]
  (get-in m [::core :range-x]))

(defn range-y
  "Returns the number of buttons on the y axis"
  [m]
  (get-in m [::core :range-y]))

(defn rand-x
  "Returns a random x coordinate"
  [m]
  (rand-nth (range (range-x m))))

(defn rand-y
  "Returns a random y coordinate"
  [m]
  (rand-nth (range (range-y m))))

(defn coords
  "Returns a lazy sequence of all pairs of x y coords"
  [m]
  (get-in m [::core :coords]))

(defn map->frame
  [m mp]
  (partition 8 (map #(get mp %) (coords m))))

(defn button-ids
  "Returns a seq of unique integers - one for each button on the monome."
  [m]
  (range (* (range-x m) (range-y m))))

(defn button-coords
  "Returns a set of coordinates matchine the id passed in. id is an int in the range of 0..num-buttons.
   This is the inverse of button-id"
  [m id]
  (let [y (int (/ id (range-x m)))
        x (- id (* (range-x m) y))]
    [x y]))

(defn button-id
  "Returns a unique integer id for a given set of coordinates. This is the inverse of button-coords"
  ([m coords] (button-id m (first coords) (second coords)))
  ([m x y]
     (+ (* (range-y m) y) x)))

;;TODO implement me
(defn map-coords
  ([m coords] (apply map-coords m coords))
  ([m x y]
     [x y]))

(defn clear
  [m]
  (monome/clear m))

(defn clear-at
  [m time]
  (monome-at/clear-at m time))

(defn all
  [m]
  (monome/all m))

(defn all-at
  [m time]
  (monome-at/all-at m time))

(defn led
  ([m coords val]
     (if (= 0 val)
       (monome/led-off m (map-coords m coords))
       (monome/led-on m (map-coords m coords))))
  ([m x y val]
     (if (= 0 val)
       (monome/led-off m (map-coords m x y))
       (monome/led-on m (map-coords m x y)))))

(defn led-at
  ([m time coords val]
     (if (= 0 val)
       (monome-at/led-off-at m time (map-coords m coords))
       (monome-at/led-on-at m time (map-coords m coords))))
  ([m time x y val]
     (if (= 0 val)
       (monome-at/led-off-at m time (map-coords m x y))
       (monome-at/led-on-at m time (map-coords m x y)))))

(defn led-on
  ([m coords]
     (monome/led-on m (map-coords m coords)))
  ([m x y]
     (monome/led-on m (map-coords m x y))))

(defn led-on-at
  ([m time coords]
     (monome-at/led-on-at m time (map-coords m coords)))
  ([m time x y]
     (monome-at/led-on-at m time (map-coords m x y))))

(defn led-off
  ([m coords]
     (monome/led-off m (map-coords m coords)))
  ([m x y]
     (monome/led-off m (map-coords m x y))))

(defn led-off-at
  ([m time coords]
     (monome-at/led-off-at m time (map-coords m coords)))
  ([m time x y]
     (monome-at/led-off-at m time (map-coords m x y))))

;;TODO implement me
(defn frame-rot
  [m]
  0)

;;TODO implement me
(defn rotate-frame
  [rot row0 row1 row2 row3 row4 row5 row6 row7]
  [row0 row1 row2 row3 row4 row5 row6 row7])

;;TODO implement me
(defn map-frame-idx
  [m idx]
  idx)

(defn frame
  ([m row0 row1 row2 row3 row4 row5 row6 row7]
     (apply monome/frame m (rotate-frame (frame-rot m) row0 row1 row2 row3 row4 row5 row6 row7))))

(defn frame-at
  ([m time row0 row1 row2 row3 row4 row5 row6 row7]
     (apply monome-at/frame-at m time (rotate-frame (frame-rot m) row0 row1 row2 row3 row4 row5 row6 row7))))


(defn light-led-on-sustain
  [m]
  (on-press m (fn [x y s] (led-on m x y)) "light led on sustain on")
  (on-release m (fn [x y s] (led-off m x y)) "light led on sustain off"))


