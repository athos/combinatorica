(ns combinatorica.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const CHUNK_SIZE 32)

(definterface SetupChunk
  (setupChunk []))

(defn- valid-indexes? [arrays indexes]
  (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays
        len1 (count arr1)
        len2 (count arr2)
        len3 (count arr3)]
    (and (< (long (nth indexes 0)) len1)
         (< (long (nth indexes 1)) len2)
         (< (long (nth indexes 2)) len3))))

(defn- adjust-indexes [arrays indexes]
  (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays
        len1 (count arr1)
        len2 (count arr2)
        len3 (count arr3)
        i1 (long (nth indexes 0))
        i2 (long (nth indexes 1))
        i3 (long (nth indexes 2))
        n (inc (+ i3 (* len3 (+ i2 (* len2 i1)))))
        i3' (mod n len3)
        n (quot n len3)
        i2' (mod n len2)
        i1' (quot n len2)]
    [i1' i2' i3']))

(deftype CartesianProduct [arrays indexes ^:volatile-mutable ^clojure.lang.IChunk _chunk ^:volatile-mutable _chunk-next ^:volatile-mutable _next]
  clojure.lang.IReduceInit
  (reduce [this ^clojure.lang.IFn f init]
    (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays
          len1 (count arr1)
          len2 (count arr2)
          len3 (count arr3)]
      (loop [i1 (long (nth indexes 0)),
             i2 (long (nth indexes 1)),
             i3 (long (nth indexes 2)),
             acc init]
        (if-not (== i3 len3)
          (if-not (== i2 len2)
            (if-not (== i1 len1)
              (let [ret (f acc [(aget arr1 i1) (aget arr2 i2) (aget arr3 i3)])]
                (if (reduced? ret)
                  @ret
                  (recur i1 i2 (inc i3) ret)))
              acc)
            (recur (inc i1) 0 0 acc))
          (recur i1 (inc i2) 0 acc)))))

  clojure.lang.Counted
  (count [this]
    (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays]
      (* (count arr1) (count arr2) (count arr3))))

  clojure.lang.Seqable
  (seq [this] this)

  clojure.lang.ISeq
  (first [this]
    (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays]
      [(aget arr1 (nth indexes 0))
       (aget arr2 (nth indexes 1))
       (aget arr3 (nth indexes 2))]))

  (more [this]
    (or (.next this) '()))

  (next [this]
    (or _next
        (do (.setupChunk this)
            (if (> (count _chunk) 1)
              (let [indexes (adjust-indexes arrays indexes)
                    chunk (.dropFirst _chunk)]
                (set! _next
                      (CartesianProduct. arrays indexes chunk _chunk-next nil))
                _next)
              (.chunkedNext this)))))

  (cons [this x]
    (clojure.core/cons x this))

  clojure.lang.IChunkedSeq
  (chunkedFirst [this]
    (.setupChunk this)
    _chunk)

  (chunkedNext [this]
    (.seq (.chunkedMore this)))

  (chunkedMore [this]
    (.setupChunk this)
    (or _chunk-next '()))

  SetupChunk
  (setupChunk [this]
    (when-not _chunk
      (let [buffer (chunk-buffer CHUNK_SIZE)
            [^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays
            len1 (count arr1)
            len2 (count arr2)
            len3 (count arr3)
            indexes' (volatile! nil)]
        (loop [i1 (long (nth indexes 0))
               i2 (long (nth indexes 1))
               i3 (long (nth indexes 2))
               c 0]
          (if-not (== i3 len3)
            (if-not (== i2 len2)
              (if-not (or (== c CHUNK_SIZE) (== i1 len1))
                (do (chunk-append buffer [(aget arr1 i1) (aget arr2 i2) (aget arr3 i3)])
                    (recur i1 i2 (inc i3) (inc c)))
                (vreset! indexes' [i1 i2 i3]))
              (recur (inc i1) 0 0 c))
            (recur i1 (inc i2) 0 c)))
        (when (and (== (count buffer) CHUNK_SIZE)
                   (valid-indexes? arrays @indexes'))
          (set! _chunk-next
                (CartesianProduct. arrays @indexes' nil nil nil)))
        (set! _chunk (chunk buffer))))))

(defn cartesian-product [& colls]
  (->CartesianProduct (mapv object-array colls)
                      (mapv (constantly 0) colls)
                      nil nil nil))
