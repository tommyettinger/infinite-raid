(ns infinite-raid.state
  (:import [squidpony.squidmath RNG LightRNG]))

(def terrain-rng (LightRNG. 36rLAND))

(def other-rng (LightRNG. 36rFIGHT))

(defn rand-int
  "Get a random int that is less than upper and greater than or equal to lower (defaulting to 0)."
  ([upper] (.nextInt other-rng (int upper)))
  ([lower upper] (.nextInt other-rng (int lower) (int upper))))

(defn rand-long
  "Get a random long that is less than upper and greater than or equal to 0."
  ([^long upper] (.nextLong other-rng upper)))

(defn rand-bits
  "Get a random number that fits in n bits."
  ([n] (.next other-rng (int n))))

(defn shuffle [coll]
  (let [n (count coll) tr (transient coll)]
    (doseq [i (range (- n 2))] (let [j (rand-int i n) switch1 (nth tr j)]
                                 (do (assoc! tr j (nth tr i))
                                     (assoc! tr i switch1))))
    (persistent! tr)))

(def versions (atom {0 {:rollback nil :trng (.getState terrain-rng) :orng (.getState other-rng)}}))

(def named-versions (atom {"The Beginning" 0}))

(def head (atom 0))

(def staging (atom {:rollback head}))

(defn reset-rngs "Set the states of both RNGs to the states remembered in the head version." []
  (.setState terrain-rng (:trng (@versions @head)))
  (.setState other-rng (:orng (@versions @head))))

(defn planned "Gets the current staging state." [] @staging)

(defn latest "Gets the latest non-staging state." [] (@versions @head))

(defn scrap-plan "Resets the staging state to the latest non-staging state."
  []
  (reset! staging (@versions @head))
  (reset-rngs))

(defn sv "Gets the Staged Value for a given name k." [k] (get @staging k nil))

(defn put "Puts a value v into the staging state with name k." [k v] (swap! staging assoc k v))

(defn modify "Takes the value in the staging state with name k, calls f with that value as its only arg,
  and changes the value in the staging state to the result of f."
  ([k f]
   (swap! staging assoc k (f (@staging k))))
  ([k f & args]
   (swap! staging assoc k (apply f (@staging k) args))))

(defn lkv "Gets the Last Known Value with name k from the latest non-staging state."
  [k] (get (@versions @head) k nil))

(defn store
  "Stores the current staging state permanently and makes it available with the given naming, if given
  one.
  Naming should not be an integer, but any other key will work. Returns the new version number."
  ([]
   (swap! staging assoc :rollback @head)
   (swap! staging assoc :trng (.getState terrain-rng))
   (swap! staging assoc :orng (.getState other-rng))
   (swap! head inc)
   (swap! versions assoc @head @staging)
   @head)
  ([naming]
   (swap! staging assoc :rollback @head)
   (swap! staging assoc :trng (.getState terrain-rng))
   (swap! staging assoc :orng (.getState other-rng))
   (swap! head inc)
   (swap! versions assoc @head @staging)
   (swap! named-versions assoc naming @head)
   @head))

(defn restore
  "Change the state to a copy of an earlier version, which can be specified by an integer version, a
  name if the version was made available with a (non-integer) naming, or not specified, which will
  revert to the previous one used. Increments the current version, does not destroy any stored states,
  and completely overwrites the staging state with the contents of the restored state, except that the
  staging state will consider the state that was current before this call to be its previous state."
  ([]
   (when (lkv :rollback)
     (swap! versions assoc (inc @head) (@versions (lkv :rollback)))
     (reset! staging (assoc (@versions (inc @head)) :rollback @head))
     (swap! head inc)
     (swap! versions assoc-in [@head :rollback] (dec @head))
     (reset-rngs)))
  ([version]
   (if (integer? version)
     (when (contains? @versions version)
       (swap! versions assoc (inc @head) (@versions version))
       (reset! staging (assoc (@versions (inc @head)) :rollback @head))
       (swap! head inc)
       (swap! versions assoc-in [@head :rollback] (dec @head)))
     (when (and (contains? @named-versions version)
                (contains? @versions (@named-versions version)))
       (swap! versions assoc (inc @head) (@versions (@named-versions version)))
       (reset! staging (assoc (@versions (inc @head)) :rollback @head))
       (swap! head inc)
       (swap! versions assoc-in [@head :rollback] (dec @head))
       (reset-rngs)))))



