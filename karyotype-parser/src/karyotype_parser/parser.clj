(ns karyotype-parser.parser
  (:use [tawny.owl])
  (:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
		[tawny.owl :as owl]
		[ncl.karyotype.human :as h]
    [ncl.karyotype.events :as e]
    [ncl.karyotype.features :as f]
    [karyotype-parser.base :as b]
  )
)


;;basic functions


;;extract chromosome and band information by detecting ISCN karyotype part.
;;can ignore abnormality type.
;;eg. return 1p33 by detecting add(1)(p33) .
;(defn Extr-loc-old
  ;"read karyotype part like add(1)(p33) and return 1p33" 
  ;[exp]
  ;(let [chrom (into [] (re-seq #"(?<=\()[X Y \d]+" exp)) 
        ;band (into [] (re-seq #"(?<=\()[p q]+[\d \.]*" exp))]
    ;(cond
      ;(and (empty? chrom) (empty? band))
      ;"CB"
      ;(and ((complement empty?) chrom) (empty? band))
      ;(str chrom "B")
      ;(and ((complement empty?) band) (empty? chrom))
      ;(str "C" band)
      ;(and ((complement empty?) band) ((complement empty?) chrom))
      ;(str (first chrom) (first band))
    ;)
  ;)
;)

;;extract band information from simple abnormality types.
(defn Extr-loc
  "read karyotype part like add(1)(p33) and return 1p33" 
  [exp]
  (let [layout (re-seq #"(?<=\()[\d X Y U p q \. \-]*(?=\))" exp)
        chrom (str/replace (first layout) "U" "C") 
        band (str/replace (if (empty? (second layout)) "U" (second layout)) "U" "B")]
    (str chrom band)
  )
)

;;BUG cannot recognise region
;;extrct chromosome and band information from more complicated ISCN karyotype part.
;;can ignore abnormality type.


;(defn Extr-mult-loc-old 
  ;"read karyotype part like t(1;3)(p33;q21) and return (\"1p33\" \"3q21\")"
  ;[exp]
  ;(let [numb (+ (/ (count (re-seq #"\;" exp)) 2) 1)
        ;chrom (into [] (re-seq #"(?<=[\; \(])[\d]+" exp)) 
        ;band (into [] (re-seq #"(?<=[\; \(])[p q]+[\d \.]*" exp))]
    ;(if (= (count chrom) (count chrom))
      ;(cond 
        ;(and (empty? chrom) (empty? band))
        ;(for [i (range numb)]
          ;"CB")
        ;(and ((complement empty?) chrom) (empty? band))
        ;(map (fn [s] (str s "B")) chrom)
        ;(and ((complement empty?) band) (empty? chrom))
        ;(map (fn [s] (str "C" s)) band)
        ;(and ((complement empty?) band) ((complement empty?) chrom))
        ;(for [i (range (count chrom))]
          ;(str (chrom i) (band i))
        ;)
      ;)
    ;)
  ;)
;)

;;extrct band information from abnormality types which involves two or more chromosomes.
(defn Extr-mult-loc 
  "read karyotype part like t(1;3)(p33;q21) and return (\"1p33\" \"3q21\")"
  [exp]
  (let [layout (re-seq #"(?<=\()[\d X Y U p q \; \.]*(?=\))" exp)
        chrom (map 
                (fn [s] (str/replace s "U" "C"))
                ;;detect the number of chromosome in the first bracket
                ;;thereby checking whether abnormality happen on the same chromsome.
                ;;eg. ins(1;7)(q11;p11p11) or ins(1)(q11q25q25) 
                (let [a (str/split (first layout) #";")]
                  (if (= (count a) 1)
                    (conj a (first a))
                    a
                  )))
        band (map
               (fn [s] (str/replace s "U" "B"))
               (if (empty? (second layout)) 
                 (for [i (range (count chrom))] "U")
                 (let [b (str/split (second layout) #";")]
                   (if (= (count b) 1)
                     (let [c (re-seq #"[p q U][\d]*" (first b))]
                       (list (first c) (apply str (rest c))))
                     b)
                   )
                 )
               )]
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
        (if (empty? band)
          (owl-class h/human "HumanChromosome")
          (owl-class h/human (str "HumanChromosomeBand" band) :subclass h/HumanChromosomeBand)
          )
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
  [region & [n]]
  (let [cleaned (str/replace region #"\-" "")
        firpa (re-find #"[X Y C \d]+" cleaned) 
        secpa (re-seq #"[p q B][\d .]*" cleaned)]
    (if n
      (if (= (count secpa) 1)
        (conj (conj secpa (first secpa)) firpa)
        (conj secpa firpa))
      (if (= (count secpa) 1)
        (map (fn [s] (str firpa s)) (conj secpa (first secpa)))
        (map (fn [s] (str firpa s)) secpa))
      )
    )
  )


;;create region entity.
;(defn Create-region
  ;"read a region like 1p21p33 and return an owl entity"
  ;[region]
  ;(let [layout (Region-divide region 1)]
    ;(owl-class b/karontology 
               ;(str "HumanChromosome" (first layout) "Band" (second layout) "=>" (last layout))
               ;:subclass b/HumanChromosomeRegion)
    ;)
  ;)
  

;;abnormality detection functions


;;detect "+" behind comma in karyotype, return a list like ("+3" "+7").
(defn Plus-rec
  "read karyotype and detect whole chromosome addition, return a list like (\"+3\" \"+7\")" 
  [karyotype]
  (re-seq #"(?<=\,\+)[\d X Y]+" karyotype))

;;create chromosome addition restriction
(defn Plus-res
  [karyotype]    
  (map 
    (fn [part] (e/addition (count (filter #{part} (Plus-rec karyotype))) (Loc-parse part))) 
    (Plus-rec karyotype))
  )


;;detect "-" behind comma in karyotype, return a list like ("-3" "-7").
(defn Minus-rec 
  "read karyotype and detect whole chromosome deletion, return a list like (\"-3\" \"-7\")"
  [karyotype]
  (re-seq #"(?<=\,)\-[\d X Y]+" karyotype))

;;create chromosome deletion restriction
(defn Minus-res
  [karyotype]
  (map 
    (fn [part] (e/deletion (count (filter #{part} (Minus-rec karyotype))) (Loc-parse part))) 
    (Minus-rec karyotype))
  )

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
(defn Del-rec 
  "read karyotype and detect band deletion on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"del[\d,X,Y,p,q,\(,\),\.,U,\-]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


(defn Del-res
  [karyotype]
  (map (fn [s]
         (if (empty? s)
           ()
           (let [countpq (count (re-seq #"[p q]" s))]
             (cond 
               (= countpq 0)
               (e/deletion 1 (Loc-parse s))
               (= countpq 1)
               (e/deletion 1 (Loc-parse s))
               (= countpq 2)
               (let [divide (Region-divide s)]
                 (e/deletion 1 (Loc-parse (first divide)) (Loc-parse (second divide)))
                 )
               )
             )
           )
         )
       (Del-rec karyotype)
    )
  )
                 
       


;;detect add(addition) in karyotype, return band name like 1p33.
(defn Add-rec 
  "read karyotype and detect band addition on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"add[\d,X,Y,U,p,q,\(,\),\.]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


(defn Add-res
  [karyotype]
  (map (fn [part] 
         (e/addition 1 (Loc-parse part))) 
       (Add-rec karyotype)
    )
  )


;;detect dup(duplication) in karyotype, return region name like 1p21p33.
(defn Dup-rec
  "read karyotype and detect region duplication on chromosome, return a region" 
  [karyotype]
  (let [sub (re-seq #"(?<=\,)dup[\d,X,Y,U,p,q,\(,\),\. \-]*\)" karyotype)] 
    (->> sub
      ;;convert "dup" to "duP" in strings to avoid misunderstanding because of "p".
      (map (fn [expr] (str/replace expr #"up" "uP")))
	    (map Extr-loc)
      (map Region-divide)
	  )
  )	
)


;;create duplication restriction
(defn Dup-res
  [karyotype]
  (map (fn [s]
         (if (empty? s)
           ()
           (let [fir (first s) sec (second s)]
             (e/duplication 1 (Loc-parse fir) (Loc-parse sec))
             )
           )
         )
       (Dup-rec karyotype)
    )
  )


;;detect trp(triplication) in karyotype, return region name like 1p21p33.
(defn Trp-rec 
  "read karyotype and detect region triplication on chromosome, return a region"
  [karyotype]
  (let [sub (re-seq #"trp[\d,X,Y,U,p,q,\(,\),\. \-]*\)" karyotype)] 
    (->> sub
      ;convert "trp" to "trP" in strings to avoid misunderstanding because of "p".
      (map (fn [expr] (str/replace expr #"rp" "rP")))
      (map Extr-loc)
      (map Region-divide)
    )
  )	
)


;;create triplication restriction
(defn Trp-res
  [karyotype]
  (map (fn [s]
         (if (empty? s)
           ()
           (let [fir (first s) sec (second s)]
             (e/triplication 1 (Loc-parse fir) (Loc-parse sec))
             )
           )
         )
      (Trp-rec karyotype)
    )
  )

;;detect qdp(quadruplication) in karyotype, return region name like 1p21p33.
(defn Qdp-rec
  "read karyotype and detect region quadruplication on chromosome, return a region"
  [karyotype]
  (let [sub (re-seq #"qdp[\d,X,Y,U,p,q,\(,\),\. \-]*\)" karyotype)] 
    (->> sub
      ;convert "qdp" to "QdP" in strings to avoid misunderstanding because of "p" and "q".
      (map (fn [expr] (str/replace expr #"qdp" "QdP")))
      (map Extr-loc)
      (map Region-divide)
    )
  )	
)


;;create Q
(defn Qdp-res
  [karyotype]
  (map (fn [s]
         (if (empty? s)
           ()
           (let [fir (first s) sec (second s)]
             (e/quadruplication 1 (Loc-parse fir) (Loc-parse sec))
             )
           )
         )
      (Qdp-rec karyotype)
    )
  )

;;detect t(translocation) in karyotype, 
;;return a list of band name like (\"1p33\" "\3q21"\).

;;(defn Tral-a [karyotype]
	;(let [firpa (fn [exp] (re-seq #"(?<=[\; \(])[\d]*" exp))
		;secpa (fn [exp] (re-seq #"[p q][\d]*" exp))]
		;(map str/join (map list (firpa karyotype) (secpa karyotype)))))

(defn Tral-rec 
  "read karyotype and detect band translocation on chromosome, return a list"
  [karyotype & [n]]
  (if n
    (let [sub (re-seq #"t[\d,X,Y,U,p,q,\(,\),\.,\;]*\)" karyotype)] 
      (map Extr-mult-loc sub)
      )
    (let [sub (re-seq #"(?<=,)t[\d,X,Y,U,p,q,\(,\),\.,\;]*\)" karyotype)] 
      (map Extr-mult-loc sub)
      )
    )
  )


;;create traslocation restriction
(defn Tral-res
  [karyotype & [n]]
  (map (fn [s]
         (if (empty? s)
           ()
           (let [fir (first s) sec (second s)]
             (e/translocation 1 [(Loc-parse fir)] [(Loc-parse sec)])
             )
           )
         )
       (Tral-rec karyotype n)
    )
  )


;;detect ins(insertion) in karyotype, 
;;return a list of band name like (\"1p33\" "\3q21"\ \"3q23\").
(defn Ins-rec 
  "read karyotype and detect band insertion on chromosome, return a list"
  [karyotype]
  (let [sub (re-seq #"(?<=,)ins[\d,X,Y,U,p,q,\(,\),\.,\;,\-]*\)" karyotype)
        extr (map Extr-mult-loc sub)]
    (map 
      (fn [s] (conj (Region-divide (second s)) (first s))) 
      extr))
  )


;;create insertion restriction

(defn Ins-res
  [karyotype]
  (map (fn [s] 
         (if (empty? s)
           ()
           (let [layout (map Loc-parse s)] 
             (e/insertion 1 (vector (first layout)) (into [] (rest layout)))
             )
           )
         ) 
      (Ins-rec karyotype)
    )
  )


;;detect inv(inversion) in karyotype, return a region like 1p21p33.
(defn Inv-rec 
  "read karyotype and detect region inversion on chromosome, return a region"
  [karyotype & [n]]
  (if n
    (let [sub (re-seq #"inv[\d,X,Y,U,p,q,\(,\),\;,\.,\-]*\)" karyotype)] 
      (map (fn [s] (Region-divide (Extr-loc s))) sub)
      )
    (let [sub (re-seq #"(?<=\,)inv[\d,X,Y,U,p,q,\(,\),\;,\.,\-]*\)" karyotype)] 
      (map (fn [s] (Region-divide (Extr-loc s))) sub)
      )
    )
  )


;;create inversion restriction
(defn Inv-res
  [karyotype & [n]]
  (map (fn [s] 
         (if (empty? s)
           ()
           (let [fir (first s) sec (second s)] 
             (e/inversion 1 (Loc-parse fir) (Loc-parse sec))
             )
           )
         )
       (Inv-rec karyotype n)
    )
  )

;;detect fis(fission) in karyotype, return a band like 1p21.
(defn Fis-rec 
  "read karyotype and detect fission on chromosome, return a band"
  [karyotype]
  (let [sub (re-seq #"fis[\d,X,Y,U,p,q,\(,\),\;,\.]*\)" karyotype)] 
    (map Extr-loc sub)
  )
)


(defn Fis-res
  [karyotype]
  (map (fn [s]
         (if (empty? s)
           ()
           (e/fission 1 (Loc-parse s))
           )
         )
       (Fis-rec karyotype)
    )
  )


(defn Der-rec
  [karyotype]
  (re-seq #"der[\( \) \d [a-z] \. \- \; X Y U]+(?=[\,]|$)"  karyotype)
  )

;;detect der in karyotype.
(defn Der-res
  [karyotype]
  (map (fn [part]
         (let [chrom (re-find #"(?<=der\()[U,\d,X,Y]+(?=\))" part)]
           (if (empty? chrom)
             ()
             (f/derivative 1 (vector (Loc-parse (str/replace chrom "U" "C")))
                           (Tral-res part 1)
                           (Inv-res part 1)
              )
             )
           )
         )
      (Der-rec karyotype)
    )
  )

;;detect isochromosome.
(defn Iso
  [karyotype]
  (let [sub (re-seq #"i[\d,X,Y,U,p,q,\(,\),\;,\.]*\)" karyotype)
        band-coll (map Extr-loc sub)]
    (map (fn [band] (e/direct-event 1 
                                    (owl-and b/isochromosome 
                                             (owl-some e/hasBreakPoint (Loc-parse band)))))
         band-coll)
    )
  )

;;detect dicentric.
(defn Dic
  [karyotype]
  (let [sub (re-seq #"dic[\d,X,Y,U,p,q,\(,\),\;,\.]*\)" karyotype)
        band-coll (map Extr-mult-loc sub)
        ]
    (map (fn [band]
           (let [fir-part (first band) sec-part (second band)] 
             (e/direct-event 1 
                             (owl-and b/dicentric 
                                      (owl-some e/hasBreakPoint (Loc-parse fir-part) (Loc-parse sec-part))))))
         band-coll)
    )
  )
                  
;;read add and create an OWL addition relationship.
;(defn Band-addition
  ;"read karyotype part and create an addition"
  ;[expr]
  
  