(ns karyotype-parser.ontology
	(:use [tawny.owl])
	(:require 
		[karyotype-parser.core :as co]
		[karyotype-parser.base :as pb]
		[tawny.read :as r]
  		[ncl.karyotype.human :as h]
    	[ncl.karyotype.events :as e]
		))
;[ncl.karyotype.human :as h]
;[ncl.karyotype.events :as e]

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
	 	(map (fn [part] (e/addition 1 (co/loc-parse part))) (co/Add (database s)))
	 	;(map (fn [part] (owl-some pb/HasAddition (co/loc-parse part))) (co/Plus (database s)))
	 	;(map (fn [part] (owl-some pb/HasDeletion (co/loc-parse part))) (co/Minus (database s)))
	 	;(map (fn [part] (owl-some pb/HasDeletion (co/loc-parse part))) (co/Del (database s)))
	 	;(map (fn [part] (owl-some pb/HasBreakPoint (co/loc-parse part))) (co/Tral (database s)))
	 	;(if (nil? (co/Tral-a (database s))) () (co/Tral-receive-parse (co/Tral-a (database s))))
	 	;(if (nil? (co/Tral-a (database s))) () (co/Tral-provide-parse (co/Tral-a (database s))))
	 	;(if (co/Idem? (database s)) (owl-some pb/HasProperty "subclones") ())
	 	;(if (co/Inc? (database s)) (owl-some pb/HasProperty "incomplete-karyotype") ())
	 	;(if (co/Mar? (database s)) (owl-some pb/HasProperty "marker-chromosome") ())
	 	;(map (fn [part] (owl-some pb/HasProperty (str "duplication" part))) (co/Dup (database s)))
	 	;(map (fn [part] (owl-some pb/HasProperty (str "triplication" part))) (co/Trp (database s)))
	 	))

;incomplete
;(defn create-class-new [s]
	;(defclass s :label (database s)
		;(map (fn [exp] (e/addition 1 `(loc-parese exp))) (co/Plus (database s)))
	;)
;)


(defn run-pipeline [] (map create-class (keys database)))

;incomplete
;(defn run-pipeline-new [] (map create-class-new (keys database)))

(defn save-karontology [] (save-ontology "ontology.owl" :owl))
