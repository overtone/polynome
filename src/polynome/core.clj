(ns polynome.core
  (:require  [monome-serial.core :as monome-core]
             [monome-serial.led :as monome]
             [monome-serial.led-at :as monome-at]
             [monome-serial.event-handlers]
             [polynome.ring-buffer :as ring])
  (:use [clojure.contrib.ns-utils :only [immigrate]]))

(immigrate
 'monome-serial.event-handlers)

;;event history
(defrecord Event [time x y action monome-name])

(def event-buf* (atom (ring/create-buf 1000)))

(defn new-event
  "Record a new event. Each event is a tuple of onset time, x and y coords, action (i.e. up, down) and monome name (or default if not supplied)."
  ([x y action]
     (new-event x y action :default))
  ([x y action name]
     (swap! event-buf* ring/insert (Event. (System/currentTimeMillis) x y action name))))

(defn find-previous
  "Returns the first event for which fun returns true or nil if no match is found."
  [fun]
  (some @event-buf* fun))

(defn init  "Initialise a monome. Raises an exception if the supplied path isn't valid or is already in use"
  [path]
  (let [monome (monome-core/connect path)]
    (on-action monome (fn [action x y] (new-event x y action)))
    (assoc monome ::polynome {:max-x 7
                              :max-y 7
                              :range-x 8
                              :range-y 8})))

(defn max-x
  "Returns the monome's maximum x coord"
  [m]
  (get-in m [::polynome :max-x]))

(defn max-y
  "Returns the monome's maximum y coord"
  [m]
  (get-in m [::polynome :max-y]))

(defn range-x
  "Returns the number of buttons on the x axis"
  [m]
  (get-in m [::polynome :range-x]))

(defn range-y
  "Returns the number of buttons on the y axis"
  [m]
  (get-in m [::polynome :range-y]))

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



