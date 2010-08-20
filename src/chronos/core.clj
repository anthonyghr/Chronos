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

;;--------------------------begin node----------------------------------------

(defrecord+id node
  [in-edges out-edges
   (lb beginning-of-time) lb-pred lb-potential-preds lb-succs
   (ub end-of-time) ub-pred ub-potential-preds ub-succs])

;;--------------------------begin constraint-----------------------------------

(defrecord+id constraint
  [active?
   lb ub
   from-node to-node
   forward-edge backward-edge])

;;--------------------------begin activity-------------------------------------

(defrecord+id activity
  [source-node finish-node])

;;--------------------------begin stn------------------------------------------

(defrecord+ stn
  [propagation-mode
   tz-node
   nodes
   constraints])

(defn propagate-sssp
  "Propagate effect of constraints using modified Bellman-Ford algorithm"
  ([stn]
     (propagate-sssp stn :earliest)
     (propagate-sssp stn :latest))
  ([stn direction]
     ))
     
  
