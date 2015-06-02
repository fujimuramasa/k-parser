(ns karyotype-parser.ontology
	(:use [tawny.owl])
	(:require 
		[karyotype-parser.core :as co]
		)
  	(:gen-class))

(defontology karyontology
  :prefix "kar:"
  :comment "An ontology modelled on an acute lymphoblastic leukemia"
  )

(defclass sample)
