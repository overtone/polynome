(ns polynome.dummy
  (:require [polynome.core :as poly])
  (:use [polynome.polynome]))

(defn- make-dummy []
  (reify Polynome
    (on-action [this f group name]
      nil)
    (clear-all-leds [this]
      nil)
    (illuminate-all-leds [this]
      nil)
    (led-on [this x y]
      nil)
    (led-off [this x y]
      nil)
    (led-frame [this idx & rows]
      nil)
    (is-connected? [this]
      true)
    (disconnect [this]
      nil)))

(defn init-dummy []
  (poly/init (make-dummy) :dummy :north 8 8))