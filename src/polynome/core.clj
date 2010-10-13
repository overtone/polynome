(ns polynome.core
  (:require  [monome-serial.core :as monome-core]
             [monome-serial.led :as monome]
             [monome-serial.led-at :as monome-at]
             [monome-serial.event-handlers :as handlers]
             [polynome.ring-buffer :as ringb])
  (:use [clojure.contrib.ns-utils :only [immigrate]]))

(defrecord Event [time x y action])

(def HISTORY-SIZE 1000)

(defn history
  [m]
  @(get-in m [::core :event-buf]))

(defn find-event
  "Returns the first event for which fun returns true or nil if no match is found."
  [m fun]
  (ringb/find-first (history m) fun))

(defn prev-event
  [m x y action]
  (find-event m #(and (= x (:x %))
                      (= y (:y %))
                      (= action (:action %)))))

(defn on-press
  ([m f] (on-press m f f))
  ([m f name] (handlers/on-press m f ::user-defined name)))

(defn on-release
  ([m f] (on-release m f f))
  ([m f name] (handlers/on-release m f ::user-defined name)))

(defn remove-handler
  [m name]
  (handlers/remove-handler m ::user-defined name))

(defn remove-all-handlers
  [m]
  (handlers/remove-group-handlers m ::user-defined))

(defn init  "Initialise a monome. Raises an exception if the supplied path isn't valid or is already in use"
  [path]
  (let [m      (monome-core/connect path)
        hist   (ringb/create-buf HISTORY-SIZE)
        poly-m (assoc m ::core {:max-x 7
                                :max-y 7
                                :range-x 8
                                :range-y 8
                                :event-buf (atom hist)})
        store-event (fn [action x y]
                      (let [event (Event. (System/currentTimeMillis) x y action)]
                        (swap! (get-in poly-m [::core :event-buf])
                               ringb/insert event)))]

    (handlers/on-action poly-m store-event ::history "store press/release history")
    poly-m))

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
  (for [y (range (range-y m))
        x (range (range-x m))]
    [x y]))

(defn map->frame
  [m mp]
  (partition 8 (map #(get mp %) (coords m))))

(defn button-id
  "Returns a unique integer id for a given set of coordinates."
  [m x y]
  (+ (* (range-y m) y) x))

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



