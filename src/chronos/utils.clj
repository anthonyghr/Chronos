(ns
    #^{:author "Anthony Gallagher",
       :doc "A macro to define records together with a factory function that gives them defaults"}
    chronos.utils
  (:use clojure.contrib.except
	[clojure.contrib.def :only (defvar)]))

(defn seq2map
  "Convert a sequence to a map"
  [key-vals]
  (->> key-vals
       (map (fn [[key val]]
	      [(keyword key) val]))
       flatten
       (apply hash-map)))

(defn make-instance
  "Make an instance of a class with the given parameters"
  [cname params]
  (eval (conj (list* params) cname 'new)))

(defmacro defrecord+
  "Defines a new record, along with a make-RecordName factory function that
   returns an instance of the record initialized with the default values
   provided as part of the record's slot declarations.  e.g.
   (defrecord+ foo
     [(a 5) b c])
   (make-foo :b 4)
   => #user.foo{:a 5, :b 4, :c nil}"
  [name slots & etc]
  (let [slots+ (for [slot slots]
		 (if (list? slot)
		   slot
		   (list slot nil)))
	fields (->> slots+ (map first) vec)
        default-map (seq2map slots+)]
    `(do
       ;;Create the record with the given name and fields
       (defrecord ~name
         ~fields
         ~@etc)

       ;;Define the constructor function
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

(defmacro defrecord+id
  "Defines a new record, adding an auto-incrementing ID field to it"
  [name slots & etc]
  (let [slots+ (conj slots 'id)]
    `(do
       ;;Create the record and constructor
       (defrecord+ ~name
	 ~slots+
	 ~@etc)

       ;;Create the counter variable
       (defvar ~(symbol (str name "-counter")) (atom 0)
	 ~(str "Keep count of the number of " name "s created"))
       
       ;;Create the specialized constructor
       (defn ~(symbol (str "make-" name "+id"))
	 ~(str "A factory function returning a new instance of " name
	       " initialized with the defaults specified in the corresponding defrecord+ form.")
	 [& body#]
	 (let [id# @~(symbol (str name "-counter"))
	       body+# (conj (list* body#) id# :id)
	       record# (apply ~(symbol (str "make-" name)) body+#)]	 
	   (swap! ~(symbol (str name "-counter")) inc)
	   record#))
       ~name)))
