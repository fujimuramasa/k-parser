(ns karyotype-parser.property
	(:use [tawny.owl])
  	(:gen-class))

(defontology karproperty
  :prefix "pro:"
  :comment "karyotype-parser properties"
  :iri "http://property"
  )

(defoproperty HasAddition)
(defoproperty HasDeletion)


