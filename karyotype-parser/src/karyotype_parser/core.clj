(ns karyotype-parser.core
  (:use [tawny.owl])
  (:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
		[tawny.owl :as owl]
		[ncl.karyotype.human :as h]
  	[ncl.karyotype.events :as e]
    [ncl.karyotype.karyotype :as k]
		[karyotype-parser.parser :as p]
  )
)


(defontology parser)
(owl-import h/human)
(owl-import e/events)

;;create an ontology for storing Acute Lymphoblastic Leukaemia karyotype.
(defontology karontology
  :prefix "kar:"
  :comment "An ontology modelled on an acute lymphoblastic leukemia"
  :iri "http://karyotype"
  )

(defclass SampleSet)


;;file handle


;;wash karyotype
(defn clean-karyotype
  "delete \"?\" and clone size symbol" 
  [karyotype]
  (-> karyotype
    (str/replace #"(?<=[\( \;])\?(?=[\) \;])" "U")
    (str/replace "?" "")
    (str/replace #"\[[C,P,\d]*\]" "")
    ;;delete subclone
    (str/replace #"/.+" "")
    (str/replace #"(?<=\,)\+(?=[\d \-]*mar)" "")
  )
)


;;parse file text format.
;;format like "3054@@46,XY[25]"
(defn make-dic 
  [line]
  (let [div (str/split line #"@@")] 
    (hash-map (first div) (clean-karyotype (get (into [] (rest div)) 0)))
  )
)


;;read file
(defn make-database 
  []
  (apply merge
    (map make-dic
      (with-open [rdr (io/reader "D:/project/parser/karyotype-parser/test.txt")]
        (doall (line-seq rdr))
      )
    )
  )
)


(def database (make-database))


(defn create-class 
  [s]
  (owl-class s :label (database s) :subclass SampleSet 
    ;;create addition restriction
    ;(map (fn [part] (e/addition 1 (p/Loc-parse part))) (p/Add (database s)))
    ;(map (fn [part] (e/addition (count (filter #{part} (p/Plus (database s)))) (p/Loc-parse part))) (p/Plus (database s)))
    ;;create deletion restriction
    ;(map (fn [part] (e/deletion 1 (p/Loc-parse part))) (p/Del (database s)))
    ;(map (fn [part] (e/deletion (count (filter #{part} (p/Minus (database s)))) (p/Loc-parse part))) (p/Minus (database s)))
    ;;create inversion restriction
    (map (fn [s] 
           (if (empty? s)
             ()
             (let [fir (first s) sec (second s)] 
               (e/inversion 1 (p/Loc-parse fir) (p/Loc-parse sec))
             )
           )
         ) 
         (p/Inv (database s))
    )
    ;;craete translocation restriction
    (map (fn [s]
           (if (empty? s)
             ()
             (let [fir (first s) sec (second s)]
               (e/translocation nil [(p/Loc-parse fir)] [(p/Loc-parse sec)])
             )
           )
         )
         (p/Tral (database s))
    )
    ;;create duplication restriction.
    ;;need cover invdup!!
    (map (fn [s]
           (if (empty? s)
             ()
             (let [fir (first s) sec (second s)]
               (e/duplication nil (p/Loc-parse fir) (p/Loc-parse sec))
             )
           )
         )
         (p/Dup (database s))
    )
    ;;create triplication restriction.
    (map (fn [s]
           (if (empty? s)
             ()
             (let [fir (first s) sec (second s)]
               (e/triplication 1 (p/Loc-parse fir) (p/Loc-parse sec))
             )
           )
         )
         (p/Trp (database s))
    )
    ;;create quadruplication restriction.
    (map (fn [s]
           (if (empty? s)
             ()
             (let [fir (first s) sec (second s)]
               (e/quadruplication 1 (p/Loc-parse fir) (p/Loc-parse sec))
             )
           )
         )
         (p/Qdp (database s))
    )
    
    
  )
)

;(defn check [s] (if (empty? (filter h/band? (map p/Loc-parse (p/Minus (database s))))) () s))


(defn run-pipeline 
  [] 
  (map create-class (keys database))
)

(defn do-pipeline
  [start end]
  (for [i (range start end)]
    (create-class (str i))
  )
)

(defn save-karontology 
  [] 
  (save-ontology "ontology.owl" :owl)
)


(defn dotest 
  [start end]
  (for [i (range start end)]
    (owl-class (str i) :label (database (str i)) :subclass SampleSet
      (map
        (fn [part] (e/addition (count (filter #{part} (p/Plus (database (str i))))) (p/Loc-parse part)))
        (p/Plus (database (str i))))
    ) 
  )
)
    