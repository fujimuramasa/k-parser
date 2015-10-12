(ns karyotype-parser.core
  (:use [tawny.owl])
  (:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
    [tawny.reasoner :as r]
		[ncl.karyotype.human :as h]
  	[ncl.karyotype.events :as e]
    [ncl.karyotype.karyotype :as k]
		[karyotype-parser.parser :as p]
  )
)

(r/reasoner-factory :elk)

;;create an ontology for storing Acute Lymphoblastic Leukaemia karyotype.
(defontology parser
  :prefix "par:"
  :comment "An ontology created for storing data"
  :iri "http://parser"
  )

(defclass Sample)

;;file handle


;;wash karyotype
(defn clean-karyotype
  "clean some symbols which may confusing recognization" 
  [karyotype]
  (-> karyotype
    ;;remove In situ Hybridization symbols which contains [wcp]
    (str/replace #"[^\(]*wcp[^\)]*" "")
    (str/replace #"[^\(]*TEL[^\)]*" "")
    (str/replace #"[^\(]*AML[^\)]*" "")
    (str/replace #"(?<=[\/ \,])[^@]*cen[^@]*(?=\[)" "")
    ;;remove empty ()
    (str/replace #"\(\)" "")
    ;;remove composite karyotype symbols (like [CP4])
    (str/replace #"\[cp[\d]*\]|\[CP[\d]*\]" "")
    ;;remove clone size numbers (like [23])
    (str/replace #"\[[\d \% \.]+\]" "")
    ;(str/replace #"\([\d]*\%\)" "")
    ;;find brackets only contain a single ? and replace these ? with U.
    (str/replace #"(?<=[\( \;])\?(?=[\) \;])" "U")
    ;;remove other ?
    (str/replace "?" "")
    ;;remove plus sign before mar
    (str/replace #"(?<=\,)\+(?=[\d \-]*mar)" "")
  )
)


;;parse file text format.
;;format should obey a format like "RegID@@Karyotype" (eg. "3054@@46,XY[25]")
(defn make-dic 
  [line]
  (if (empty? (re-find #"@@" line))
    ()
    (let [div (str/split line #"@@")] 
      (hash-map (first div) (clean-karyotype (second div)))
      )
    )
  )


;;read file
;;change file name in the following string "import.txt".
(defn make-database 
  []
  (apply merge
    (map make-dic
      (with-open [rdr (io/reader "LRCG.txt")]
        (doall (line-seq rdr))
      )
    )
  )
)


(def database (make-database))

;;create entites with restrictions.
(defn create-class
  ;;s is RegID, kar is karyotype string
  [s kar]
  (owl-class (str/replace kar " " "") :label s :subclass Sample
    ;(map (fn [part] (e/addition 1 (p/Loc-parse part))) (p/Add kar))
    (p/Add-res kar)          
    (p/Plus-res kar)
    ;;create deletion restriction
    ;(map (fn [part] (e/deletion 1 (p/Loc-parse part))) (p/Del kar))
    (p/Del-res kar)
    (p/Minus-res kar)
    ;;create inversion restriction
    (p/Inv-res kar)
    ;;craete translocation restriction
    (p/Tral-res kar)
    ;;create duplication restriction.
    (p/Dup-res kar)
    ;;create triplication restriction.
    (p/Trp-res kar)
    ;;create quadruplication restriction.
    (p/Qdp-res kar)
    ;;create insertion restriction.
    (p/Ins-res kar)
    ;;create derivative restriction.
    (p/Der-res kar)
    ;;create fission restriction.
    (p/Fis-res kar)
    ;;create isochromosome restriction.
    (p/Iso kar)
    ;;create dicentric restriction.
    (p/Dic kar)
  )
)


;;run above create-class function for each subclone of karyotype.
(defn create-class-with-check-subclone
  [s]
  (let [layout (str/split (database s) #"/")]
    (if (= (count layout) 1)
      ;(owl-class s :label (database s) :subclass Sample
                 ;(build-restriction (database s)))
      (create-class s (database s))
      (for [i (range (count layout))]
        (let [numb (str s "-" i)
             kar (layout i)]
        ;(owl-class (str s "k" i) :label (layout i) 
        ;           :subclass (owl-class s :subclass Sample)
        ;           (build-restriction (layout i))
        ;  )
          (create-class numb kar)
          )
        )
      )
    )
  )
        

;;parse all input strings and put them into an ontology.
(defn full-pipeline 
  [] 
  (map create-class-with-check-subclone (keys database))
)

;;parse selected range of strings into an ontology.(eg. (do-pipeline [1 1000]))
(defn range-pipeline
  [start end]
  (for [i (range start end)]
    (if (contains? database (format "%04d" i))
    (create-class-with-check-subclone (format "%04d" i))
    )
  )
)



;;parse karyotype strings in a list of regid number.(eg. (test-pipeline [1001 1002 1003]))
(defn select-pipeline
  [rang]
  (for [i rang]
    (if (contains? database (format "%04d" i))
    (create-class-with-check-subclone (format "%04d" i))
    )
    )
  )

;;test random number karyotype.
(defn random-pipeline
  [numb]
  (let [str-to-num (fn [s] (Integer. s))]
    (select-pipeline 
      (map str-to-num 
           (repeatedly numb #(rand-nth (keys database)))
           )
      )
    )
  )

;;save ontology
(defn save-karontology 
  [] 
  (save-ontology "ontology.owl" :owl)
)

;;test fucntion.
;(defn dotest 
  ;[start end]
  ;(for [i (range start end)]
    ;(owl-class (str i) :label (database (str i)) :subclass Sample
      ;(p/Der (database (str i)))
    ;) 
  ;)
;)
    
;(defn -main [] (let [a (random-pipeline 2000)] (time (r/consistent?))) )