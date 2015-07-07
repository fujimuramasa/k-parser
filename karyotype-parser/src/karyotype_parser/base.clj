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


(defclass HumanChromosomeRegion)

(defn Region-divide
  "read a region like 1p21p33 and return (\"1p21\" \"1p33\")" 
  [region]
  (let [firpa (re-find #"[X Y \d]+" region) secpa (re-seq #"[p q][\d .]*" region)]
    (if (= (count secpa) 1)
      (conj (conj secpa (first secpa)) firpa)
      (conj secpa firpa)
    )
  )
)


;;create region entity.
(defn Create-region
  "read a region like 1p21p33 and return an owl entity"
  [region]
  (let [layout (Region-divide region)]
    (owl-class karontology 
               (str "HumanChromosome" (first layout) "Band" (second layout) "to" (last layout))
               :subclass HumanChromosomeRegion)
    )
  )

(defn addition-region
  [region]
  (owl-and e/Addition
           (owl-some e/hasBreakPoint (Create-region region)))
  )
  
 (defn addition-new
   [n region]
   (e/direct-event n (addition-region region)))
 
 
  

