(ns
    #^{:author "Anthony Gallagher",
       :doc "A Simple Temporal Network (STN) implementing the TIME96 Cesta & Oddi algorithm"}
    chronos.core
  (:use clojure.contrib.except
	[clojure.contrib.def :only (defvar)]))

;;Constants
(defvar beginning-of-time -100000000
  "Holds the beginning-of-time value: The minimum time value that a time point can obtain")
(defvar end-of-time (- Integer/MAX_VALUE 400000000)
  "Holds the end-of-time value: The maximum time value that a time point can obtain")
(defvar pos-infinity (- Integer/MAX_VALUE 400000000)
  "The maximum allowed time value")
(defvar neg-infinity (+ Integer/MIN_VALUE 400000000)
  "The minimum allowed time value")

;;Define a function to make an edge
(defrecord edge
  [constraint from-node to-node weight])

(defmacro make-edge
  "Create a graph edge given a map of parameters. The returned edge is simply a map with the edge info"
  [& body]
  `(let [edgemap# (hash-map ~@body)]
     ;;Make the edge
     (edge. (get edgemap# :constraint nil)
	    (get edgemap# :from-node nil)
	    (get edgemap# :to-node nil)
	    (get edgemap# :weight 0))))

;;Define a function to make a node
(defrecord node
  [id in-edges out-edges
   lb lb-pred lb-potential-preds lb-succs
   ub ub-pred ub-potential-preds ub-succs])

(let [node-id (atom 0)]
  (defmacro make-node
    "Create a graph node given a map with parameters."
    [& body]
    ;;Make the node
    `(let [nodemap# (hash-map ~@body)]
       (let [node (node. (get nodemap# :id @node-id)
			 (get nodemap# :in-edges #{})
			 (get nodemap# :out-edges #{})
			 (get nodemap# :lb beginning-of-time)
			 (get nodemap# :lb-pred nil)
			 (get nodemap# :lb-potential-preds #{})
			 (get nodemap# :lb-succs #{})
			 (get nodemap# :ub end-of-time)
			 (get nodemap# :ub-pred nil)
			 (get nodemap# :ub-potential-preds #{})
			 (get nodemap# :ub-succs #{}))]
	 (swap! node-id inc)
	 node))))

;;--------------------------begin constraint-----------------------------------

(defrecord constraint
  [id active?
   lb ub
   from-node to-node
   forward-edge backward-edge])

(let [constraint-id (atom 0)]
  (defmacro make-constraint
    "Create a temporal constraint given a map with parameters."
    [& body]
    ;;Make the node
    `(let [constraintmap# (hash-map ~@body)]
       (let [node (node. (get constraintmap# :id @constraint-id)
			 (get constraintmap# :active? false)
			 (get constraintmap# :lb nil)
			 (get constraintmap# :ub nil)
			 (get constraintmap# :from-node nil)
			 (get constraintmap# :to-node nil)
			 (get constraintmap# :forward-edge nil)
			 (get constraintmap# :backward-edge nil))]
	 (swap! node-id inc)
	 node))))

;;--------------------------begin activity-------------------------------------

(defrecord activity
  [source-node finish-node])

(defmacro make-activity
  "Create a temporal activity given a map with parameters."
  [& body]
  `(let [activitymap# (hash-map ~@body)]
     (activity. (get activitymap# :source-node (make-node))
		(get activitymap# :finish-node (make-node)))))

;;--------------------------begin stn------------------------------------------

(defrecord stn
  [propagation-mode
   tz-node
   nodes
   constraints])

(defmacro make-stn
  "Create the STN"
  [& body]
  `(let [stnmap# (hash-map ~@body)]
     (stn. (get stnmap# :propagation-mode :auto)
	   (get stnmap# :tz-node (make-node :lb beginning-of-time
					    :ub beginning-of-time))
	   (get stnmap# :nodes {})
	   (get stnmap# :constraints {}))))
