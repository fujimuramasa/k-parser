(ns karyotype-parser.parser
  (:use [tawny.owl])
  (:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
		[tawny.owl :as owl]
		[ncl.karyotype.human :as h]
    [karyotype-parser.base :as b]
  )
)


;;basic functions


;;extract chromosome and band information by detecting ISCN karyotype part.
;;can ignore abnormality type.
;;eg. return 1p33 by detecting add(1)(p33) .
(defn Extr-loc-old
  "read karyotype part like add(1)(p33) and return 1p33" 
  [exp]
  (let [chrom (into [] (re-seq #"(?<=\()[X Y \d]+" exp)) 
        band (into [] (re-seq #"(?<=\()[p q]+[\d \.]*" exp))]
    (cond
      (and (empty? chrom) (empty? band))
      "CB"
      (and ((complement empty?) chrom) (empty? band))
      (str chrom "B")
      (and ((complement empty?) band) (empty? chrom))
      (str "C" band)
      (and ((complement empty?) band) ((complement empty?) chrom))
      (str (first chrom) (first band))
    )
  )
)

(defn Extr-loc
  "read karyotype part like add(1)(p33) and return 1p33" 
  [exp]
  (let [layout (re-seq #"(?<=\()[\d X Y U p q \.]*(?=\))" exp)
        chrom (str/replace (first layout) "U" "C") 
        band (str/replace (if (empty? (second layout)) "U" (second layout)) "U" "B")]
    (str chrom band)
  )
)

;;BUG cannot recognise region
;;extrct chromosome and band information from more complicated ISCN karyotype part.
;;can ignore abnormality type.
;;eg. return a list containing 1p23 and 3q12 by detecting t(1;3)(p33;q21)
;(defn Extr-mult-loc-old 
  ;"read karyotype part like t(1;3)(p33;q21) and return (\"1p33\" \"3q21\")"
  ;[exp] 
  ;(if (empty? (re-find #"[p q]" exp))
    ;(re-seq #"(?<=[\; \(])[\d]*" exp)
    ;(let [firpa (fn [part] (re-seq #"(?<=[\; \(])[\d]*" part))
		;secpa (fn [part] (re-seq #"[p q][\d \.]*" part))]
      ;(map str/join (map list (firpa exp) (secpa exp))))))

(defn Extr-mult-loc-old 
  "read karyotype part like t(1;3)(p33;q21) and return (\"1p33\" \"3q21\")"
  [exp]
  (let [numb (+ (/ (count (re-seq #"\;" exp)) 2) 1)
        chrom (into [] (re-seq #"(?<=[\; \(])[\d]+" exp)) 
        band (into [] (re-seq #"(?<=[\; \(])[p q]+[\d \.]*" exp))]
    (if (= (count chrom) (count chrom))
      (cond 
        (and (empty? chrom) (empty? band))
        (for [i (range numb)]
          "CB")
        (and ((complement empty?) chrom) (empty? band))
        (map (fn [s] (str s "B")) chrom)
        (and ((complement empty?) band) (empty? chrom))
        (map (fn [s] (str "C" s)) band)
        (and ((complement empty?) band) ((complement empty?) chrom))
        (for [i (range (count chrom))]
          (str (chrom i) (band i))
        )
      )
    )
  )
)


(defn Extr-mult-loc 
  "read karyotype part like t(1;3)(p33;q21) and return (\"1p33\" \"3q21\")"
  [exp]
  (let [layout (re-seq #"(?<=\()[\d X Y U p q \; \.]*(?=\))" exp)
        chrom (map 
                (fn [s] (str/replace s "U" "C"))
                (str/split (first layout) #";"))
        band (map
               (fn [s] (str/replace s "U" "B"))
               (if (empty? (second layout)) 
                 (for [i (range (count chrom))] "U")
                 (str/split (second layout) #";")
               ))]
  (for [z (range (count chrom))]
    (str (nth chrom z) (nth band z)))  
  )
)

;;check whether imported value exist in tawny karyotype human ontology.
(defn Tawny-exist?
  "check the validty of HumanChromosome, return true or nil"
  [s]
  (or (h/band? s) (h/chromosome? s))
)


;;read an expression like 1p33 and parse it by tawny karyotype human ontology.
(defn Loc-parse 
  "read expression like 1p33 and return HumanChromosome1Bandp33"
  [loc]
  ;(if (empty? loc)
    ;()
    (let [chrom (re-find #"[\d,X,Y,C]+" loc) 
        band (re-find #"[p,q][\d+,\.,ter]*|B" loc) 
        ]
      (cond
        (and (= chrom "C") (= band "B"))
        (owl-class h/human "HumanChromosomeBand" :subclass h/HumanChromosomeBand)
        (and (not= chrom "C") (= band "B"))
        (owl-class h/human (str "HumanChromosome" chrom "Band") :subclass h/HumanChromosomeBand)
        (and (= chrom "C") (not= band "B"))
        (owl-class h/human (str "HumanChromosomeBand" band) :subclass h/HumanChromosomeBand)
        :else
        (let [not-empty? (complement empty?)
              entity (str (if (not-empty? chrom) (str "HumanChromosome" chrom))
                          (if (not-empty? band) (str "Band" band)))]
          (if (Tawny-exist? entity)
            (owl-class h/human entity)
            (owl-class h/human 
                       (str "HumanChromosome" chrom "Band" (re-find #"[p q]+" band)) 
                       :subclass h/HumanChromosomeBand)
          )
        )
      )
    )
  ;)
)


;;check whether imported value is a valid chromosome or band.
(defn Valid?
  "check the validty of HumanChromosome, return true or false"
  [s]
  (or (h/band? s) (h/chromosome? s))
)


;;parser region, read a region like 1p21p33,
;;and return its start and end band in a list.
(defn Region-divide
  "read a region like 1p21p33 and return (\"1p21\" \"1p33\")" 
  [n region]
  (let [firpa (re-find #"[X Y \d]+" region) secpa (re-seq #"[p q][\d .]*" region)]
    (cond 
      (= n 1)
      (if (= (count secpa) 1)
        (conj (conj secpa (first secpa)) firpa)
        (conj secpa firpa))
      (= n 2)
      (if (= (count secpa) 1)
        (map (fn [s] (str firpa s)) (conj secpa (first secpa)))
        (map (fn [s] (str firpa s)) secpa))
      )
    )
  )


;;create region entity.
(defn Create-region
  "read a region like 1p21p33 and return an owl entity"
  [region]
  (let [layout (Region-divide region)]
    (owl-class b/karontology 
               (str "HumanChromosome" (first layout) "Band" (second layout) "=>" (last layout))
               :subclass b/HumanChromosomeRegion)
    )
  )
  

;;abnormality detection functions


;;detect "+" behind comma in karyotype, return a list like ("+3" "+7").
(defn Plus
  "read karyotype and detect whole chromosome addition, return a list like (\"+3\" \"+7\")" 
  [karyotype]
  (re-seq #"(?<=\,\+)[\d X Y]+" karyotype))


;;detect "-" behind comma in karyotype, return a list like ("-3" "-7").
(defn Minus 
  "read karyotype and detect whole chromosome deletion, return a list like (\"-3\" \"-7\")"
  [karyotype]
  (re-seq #"(?<=\,)\-[\d X Y]+" karyotype))


;;detect Fail condition.
(defn Fail? 
  "read karyotype and return true or false"
  [karyotype]
  (if (re-find #"Fail" karyotype)
    true
    false
  )
)


;;detect Idem (stemline karyotype in a subclone).
(defn Idem? 
  "read karyotype and return true or false"
  [karyotype]
  (if (re-find #"idem" karyotype)
    true
    false
  )
)


;;detect +mar (mark chromosome).
(defn Mar?
  "read karyotype and return true or false" 
  [karyotype]
  (if (re-find #"mar" karyotype)
    true
    false
  )
)


;;detect inc (incomplete karyotype).
(defn Inc?
  "read karyotype and return true or false" 
  [karyotype]
  (if (re-find #"inc" karyotype)
    true
    false
  )
)


;;detect del(deletion) in karyotype, return band name like 1p33.
(defn Del 
  "read karyotype and detect band deletion on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"del[\d,X,Y,p,q,\(,\),\.]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


;;detect add(addition) in karyotype, return band name like 1p33.
(defn Add 
  "read karyotype and detect band addition on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"add[\d,X,Y,p,q,\(,\),\.]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


;;detect dup(duplication) in karyotype, return region name like 1p21p33.
(defn Dup
  "read karyotype and detect region duplication on chromosome, return a region" 
  [karyotype]
  (let [sub (re-seq #"(?<=\,)dup[\d,X,Y,p,q,\(,\),\.]*\)" karyotype)] 
    (->> sub
      ;;convert "dup" to "duP" in strings to avoid misunderstanding because of "p".
      (map (fn [expr] (str/replace expr #"up" "uP")))
	    (map Extr-loc)
      (map Region-divide)
	  )
  )	
)


;;detect trp(triplication) in karyotype, return region name like 1p21p33.
(defn Trp 
  "read karyotype and detect region triplication on chromosome, return a region"
  [karyotype]
  (let [sub (re-seq #"trp[\d,X,Y,p,q,\(,\),\.]*\)" karyotype)] 
    (->> sub
      ;convert "trp" to "trP" in strings to avoid misunderstanding because of "p".
      (map (fn [expr] (str/replace expr #"rp" "rP")))
      (map Extr-loc)
      (map Region-divide)
    )
  )	
)


;;detect qdp(quadruplication) in karyotype, return region name like 1p21p33.
(defn Qdp
  "read karyotype and detect region quadruplication on chromosome, return a region"
  [karyotype]
  (let [sub (re-seq #"qdp[\d,X,Y,p,q,\(,\),\.]*\)" karyotype)] 
    (->> sub
      ;convert "qdp" to "QdP" in strings to avoid misunderstanding because of "p" and "q".
      (map (fn [expr] (str/replace expr #"qdp" "QdP")))
      (map Extr-loc)
      (map Region-divide)
    )
  )	
)


;;detect t(translocation) in karyotype, 
;;return a list of band name like (\"1p33\" "\3q21"\).

;;(defn Tral-a [karyotype]
	;(let [firpa (fn [exp] (re-seq #"(?<=[\; \(])[\d]*" exp))
		;secpa (fn [exp] (re-seq #"[p q][\d]*" exp))]
		;(map str/join (map list (firpa karyotype) (secpa karyotype)))))

(defn Tral 
  "read karyotype and detect band translocation on chromosome, return a list"
  [karyotype]
  (let [sub (re-seq #"(?<=,)t[\d,X,Y,p,q,\(,\),\.,\;]*\)" karyotype)] 
    (map Extr-mult-loc sub)
  )
)


;;detect inv(inversion) in karyotype, return a region like 1p21p33.
(defn Inv 
  "read karyotype and detect region inversion on chromosome, return a region"
  [karyotype]
  (let [sub (re-seq #"inv[\d,X,Y,p,q,\(,\),\;,\.]*\)" karyotype)] 
    (map (fn [s] (Region-divide (Extr-loc s))) sub)
  )
)


;;detect fis(fission) in karyotype, return a band like 1p21.
(defn Fis 
  "read karyotype and detect fission on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"fis[\d,X,Y,p,q,\(,\),\;,\.]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


;;read add and create an OWL addition relationship.
;(defn Band-addition
  ;"read karyotype part and create an addition"
  ;[expr]
  
  