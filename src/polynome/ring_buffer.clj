(ns polynome.ring-buffer)

(defrecord RingBuffer [buf size current initial])

(defn create-buf
  "Create a new ring buffer with the specified size"
  [size]
  (let [buf (into [] (take size (repeat nil)))]
    (RingBuffer. buf size 0 0)))

(defn next-index
  "Returns the index to the next slot in the buffer based on the current index - looping around the ring if necessary"
  [ring]
  (mod (inc (:current ring)) (:size ring)))

(defn nth-prev-index
  "Returns the index to the nth previous slot in the buffer based on the current index - looping around the ring if necessary."
  [ring n]
  (mod (- (:current ring) n) (:size ring)))

(defn insert
  "Insert a new value into the ring buffer"
  [ring val]
  (let [next-idx (next-index ring)
        new-buf (assoc (:buf ring) next-idx val)]
    (RingBuffer. new-buf (:size ring) next-idx (:start ring))))

(defn curr
  "Returns the previous item"
  [ring]
  (nth (:buf ring) (:current ring)))

(defn nth-prev
  "Returns the nth previous item. If n is >= the ring's size (or limit if supplied) or the ring doesn't yet contain n items, returns nil."
  ([ring n] (nth-prev ring n (:size ring)))
  ([ring n limit]
     (if (>= n limit)
       nil
       (let [idx (nth-prev-index ring n)]
         (nth (:buf ring) idx)))))

(defn prev
  "Returns the previous item from the ring buffer or nil if the buffer is already empty."
  [ring]
  (nth-prev ring 1))

(defn find-first
  "Returns the first value for which the fun returns true otherwise nil."
  [ring fun]
  (loop [num-back 0]
    (if-let [prev (nth-prev ring num-back)]
      (if (fun prev)
        prev
        (recur (inc num-back)))
      nil)))
