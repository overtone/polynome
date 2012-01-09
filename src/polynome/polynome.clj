(ns polynome.polynome)

(defprotocol Polynome
  (on-action [this f group name])
  (clear-all-leds [this])
  (illuminate-all-leds [this])
  (led-on [this x y])
  (led-off [this x y])
  (led-frame [this idx & rows])
  (is-connected? [this])
  (disconnect [this]))

