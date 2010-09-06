(ns
    #^{:author "Anthony Gallagher",
       :doc "A Simple Temporal Network (STN) implementing the TIME96 Cesta & Oddi algorithm"}
    chronos.core
  (:use chronos.utils
	clojure.contrib.except
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

;;--------------------------begin edge----------------------------------------

(defrecord+id edge
  [from-node to-node
   (weight 0)
   constraint])

;;--------------------------begin constraint-----------------------------------

(defrecord+id constraint
  [active?
   lb ub
   from-node to-node
   forward-edge backward-edge])


;;--------------------------begin node----------------------------------------

(defrecord+id node
  [;;Edges going into and out of the node
   (in-edges (ref #{}))
   (out-edges (ref #{}))

   ;;The lower bound
   (lb (ref beginning-of-time))
   ;;The prececessor node and edge
   (lb-pred (ref nil))
   (lb-pred-edge (ref nil))
   ;;The successor nodes
   (lb-succs (ref #{}))

   ;;The upper bound
   (ub (ref end-of-time))
   ;;The predecessor node and edge
   (ub-pred (ref nil))
   (ub-pred-edge (ref nil))
   ;;The successor nodes
   (ub-succs (ref #{}))])


;;--------------------------begin activity-------------------------------------

(defrecord+id activity
  [(source-node (make-node+id))
   (finish-node (make-node+id))])

;;--------------------------begin stn------------------------------------------

(defrecord+ stn
  [(propagation-mode (ref :incremental))
   (tz-node (ref (make-node+id :lb (ref beginning-of-time) :ub (ref beginning-of-time))))
   (nodes (ref {}))
   (constraints (ref {}))])

(defn propagate-sssp
  "Propagate effect of constraints using modified Bellman-Ford algorithm"
  ([stn]
     (propagate-sssp stn :earliest)
     (propagate-sssp stn :latest))
  ([stn direction]
     ))
     
  
