(ns karyotype-parser.ontology
	(:use [tawny.owl])
	(:require 
		[karyotype-parser.core :as co]
		[ncl.karyotype.human :as h]
		[ncl.karyotype.events :as e]
		[tawny.read :as r]
		)
  	(:gen-class))

(defontology karontology
  :prefix "kar:"
  :comment "An ontology modelled on an acute lymphoblastic leukemia"
  :iri "http://karyotype"
  )

(defoproperty hasPart)

;(def t (symbol "h/HumanChromosomeX"))

(defclass VD)

(defclass sampleset)

(def database (co/make-database 1))

(defn create-class [s]
	 (owl-class s :label (database s) :subclass sampleset (owl-some e/hasProvidingBreakPoint h/HumanChromosomeX)))

(defn run-pipeline [] (map create-class (keys database)))

(defn save-karontology [] (save-ontology "ontology.owl" :owl))
