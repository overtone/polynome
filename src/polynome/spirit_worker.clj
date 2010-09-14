(ns polynome.spirit-worker
  (:import (java.util.concurrent LinkedBlockingQueue)))

(defn spirit
  "Create a new spirit that inhabits its own separate thread"
  []
  (let [queue (LinkedBlockingQueue.)
        work #(loop [job (.take queue)]
                (apply job [])
                (recur (.take queue)))
        worker (Thread. work)]
    (.start worker)
    {:queue queue
     :worker worker}))

(defn elicit
  "Send the argless fn job to the spirit for evaluation on a separate thread"
  [spirit job]
  (.put (:queue spirit) job))
