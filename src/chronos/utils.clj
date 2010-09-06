(ns
    #^{:author "Anthony Gallagher",
       :doc "A macro to define records together with a factory function that gives them defaults"}
    chronos.utils
  (:use clojure.contrib.except
	[clojure.contrib.def :only (defvar)]))

(defn ensure-key-params
  "Makes sure the arguments are keys - (:a 1 :b 2)"
  [key-vals]
  (->> key-vals
       (map (fn [[key val]]
	      [(keyword key) val]))
       (apply concat)))

(defmacro make-instance
  "Creates an instance of a record based on the passed arguments and the default arguments"
  [cname fields user-vals default-vals]
  `(let [user-map# (apply hash-map ~user-vals)
	 default-map# (apply hash-map ~default-vals)
	 record-vals# (list*
		       (for [rawkey# '~fields]
			 (let [key# (keyword rawkey#)]
			   (get user-map# key# (default-map# key#)))))]
     (eval (conj record-vals# ~cname 'new))))

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
        default-vals (ensure-key-params slots+)]
    `(do
       ;;Create the record with the given name and fields
       (defrecord ~name
         ~fields
         ~@etc)

       ;;Define the constructor macro
       (defmacro ~(symbol (str "make-" name))
         ~(str "A factory function returning a new instance of " name
	       " initialized with the defaults specified in the corresponding defrecord+ form.")
         [& user-vals#]
	 (let [name# ~name
	       fields# '~fields
	       default-vals# '~default-vals]
	   `(make-instance ~name# ~fields# '~user-vals# '~default-vals#)))
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
       (defmacro ~(symbol (str "make-" name "+id"))
	 ~(str "A factory function returning a new instance of " name
	       " initialized with the defaults specified in the corresponding defrecord+ form.")
	 [& user-vals#]
	 (let [id# (deref ~(symbol (str name "-counter")))
	       user-vals+# (conj (list* user-vals#) id# :id)
	       record# (conj user-vals+# '~(symbol (str "make-" name)))]
	   (swap! ~(symbol (str name "-counter")) inc)
	   record#))
       ~name)))
