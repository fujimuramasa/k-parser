(ns karyotype-parser.base
	(:use [tawny.owl])
 	(:require 
		[ncl.karyotype.events :as e]
  	)
 )

(defontology karontology
  :prefix "kar:"
  :comment "karyotype-parser ontologies"
  :iri "http://karyotype"
  )


(defclass isochromosome)
(defclass dicentric) 
 
  

