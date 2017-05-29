(ns combinatorica.core)

(set! *unchecked-math* :warn-on-boxed)

(deftype CartesianProduct [arrays]
  clojure.lang.IReduceInit
  (reduce [this ^clojure.lang.IFn f init]
    (let [[^"[Ljava.lang.Object;" arr1 ^"[Ljava.lang.Object;" arr2 ^"[Ljava.lang.Object;" arr3] arrays
          len1 (count arr1)
          len2 (count arr2)
          len3 (count arr3)]
      (loop [i1 0, i2 0, i3 0, acc init]
        (if-not (== i3 len3)
          (if-not (== i2 len2)
            (if-not (== i1 len1)
              (let [ret (f acc [(aget arr1 i1) (aget arr2 i2) (aget arr3 i3)])]
                (if (reduced? ret)
                  @ret
                  (recur i1 i2 (inc i3) ret)))
              acc)
            (recur (inc i1) 0 0 acc))
          (recur i1 (inc i2) 0 acc))))))

(defn cartesian-product [& colls]
  (->CartesianProduct (mapv object-array colls)))
