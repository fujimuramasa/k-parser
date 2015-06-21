(ns karyotype-parser.ontology
	(:use [tawny.owl])
	(:require 
		[karyotype-parser.core :as co]
		[karyotype-parser.property :as p]
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

(defclass SampleSet)
;(defclass HumanChromosome)

;(doseq
        ;[n ["X" "Y" "1" "2" "3"
            ;"4" "5" "6" "7" "8"
            ;"9" "10" "11" "12" 
            ;"13" "14" "15" "16"
            ;"17" "18" "19" "20"
            ;"21" "22"]]
      ;(owl-class (str "HumanChromosome" n) :subclass HumanChromosome))

(def database (co/make-database 1))

(defn create-class [s]
	(owl-class s :label (database s) :subclass SampleSet 
	 	;(map (fn [part] (owl-some p/HasAddition (co/loc-parse part))) (co/Add (database s)))
	 	;(map (fn [part] (owl-some p/HasAddition (co/loc-parse part))) (co/Plus (database s)))
	 	(map (fn [part] (owl-some p/HasDeletion (co/loc-parse part))) (co/Minus (database s)))
	 	(map (fn [part] (owl-some p/HasDeletion (co/loc-parse part))) (co/Del (database s)))))


(defn run-pipeline [] (map create-class (keys database)))

(defn save-karontology [] (save-ontology "ontology.owl" :owl))
