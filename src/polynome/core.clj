(ns polynome.core
  (:require  [monome-serial.core :as monome-core]
             [monome-serial.led :as monome]
             [monome-serial.led-at :as monome-at]
             monome-serial.event-handlers)
  (:use [clojure.contrib.ns-utils :only [immigrate]]))

(immigrate
 'monome-serial.event-handlers)

(defn init  "Initialise a monome. Raises an exception if the supplied path isn't valid or is already in use"
  [path]
  (let [monome (monome-core/connect path)]
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
  (for [x (range (range-x m))
        y (range (range-y m))]
    [x y]))

(defn button-id
  "Returns a unique integer id for a given set of coordinates."
  [m x y]
  (+ (* (range-y m) y) x))

;;TODO implement me
(defn map-coords
  ([m coords] (apply map-coords m coords))
  ([m x y]
     [x y]))

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


