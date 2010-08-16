(ns
    #^{:author "Anthony Gallagher",
       :doc "A macro to define records together with a factory function that gives them defaults"}
    chronos.utils
  (:use clojure.contrib.except))

(defn- seq2map
  "Convert a sequence to a map"
  [key-vals]
  (loop [dmap {}
	 key-vals key-vals]
    (if (empty? key-vals)
      dmap
      (let [elem (first key-vals)]
	(recur (assoc dmap (keyword (first elem)) (second elem))
	       (rest key-vals))))))

(defn- make-instance
  "Make an instance of a class with the given parameters"
  [cname params]
  (eval (conj (list* params) cname 'new)))

(defmacro defrecord+
  "Defines a new record, along with a make-RecordName factory function that
   returns an instance of the record initialized with the default values
   provided as part of the record's slot declarations.  e.g.
   (defrecord+ foo
     [(a 5) b])
   (new-foo)
   => #user.foo{:a 5, :b nil}"
  [name slots & etc]
  (let [slots+ (for [slot slots]
		 (if (list? slot)
		   slot
		   (list slot nil)))
	fields (->> slots+ (map first) vec)
        default-map (seq2map slots+)]
    `(do
       (defrecord ~name
         ~fields
         ~@etc)
       (defn ~(symbol (str "make-" name))
         ~(str "A factory function returning a new instance of " name
	       " initialized with the defaults specified in the corresponding defrecord+ form.")
         [& body#]
	 (let [user-map# (seq2map (partition 2 body#))
	       record-vals# (for [rawkey# '~fields]
			      (let [key# (keyword rawkey#)]
				(get user-map# key# (~default-map key#))))]
	    (make-instance ~name record-vals#)))
       ~name)))
