(ns karyotype-parser.base
	(:use [tawny.owl])
 )

(defontology karproperty
  :prefix "pro:"
  :comment "karyotype-parser properties"
  :iri "http://base"
  )

(defoproperty HasAddition)
(defoproperty HasDeletion)
(defoproperty HasBreakPoint)
(defoproperty IsLocatedOn)
(defoproperty HasTranslocation)
(defoproperty HasProvidePart)
(defoproperty HasReceivePart)
(defoproperty HasProperty)


;(map (fn [chrom] (defclass (symbol (str "HumanChromosome" chrom)))) 
	;["X" "Y" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22"])


