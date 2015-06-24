(ns karyotype-parser.core
	(:use [tawny.owl])
	(:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
		[tawny.owl :as owl]
		[ncl.karyotype.human :as h]
		[karyotype-parser.base :as pb]
		))

;basic functions
(defn Extr-loc [exp]
	(str/join (re-seq #"[\d,p,q,\.]" exp))
)

;(defn Extr-mult-loc [exp]
	;(let [not-empty? (complement empty?)
		;coll (into [] (filter not-empty? (re-seq #"[\d,p,q,\.]*" exp)))
		;len (count coll)]
			;(map (fn [x] (str/join [(nth coll x) (nth coll (+ x (/ len 2)))])) (range (/ len 2)))
	;)
;)

(defn Extr-mult-loc [exp] 
	(if (empty? (re-find #"[p q]" exp))
		(re-seq #"(?<=[\; \(])[\d]*" exp)
		(let [firpa (fn [part] (re-seq #"(?<=[\; \(])[\d]*" part))
			secpa (fn [part] (re-seq #"[p q][\d]*" part))]
			(map str/join (map list (firpa exp) (secpa exp))))))

;(defn loc-parse [loc]
	;(let [chrom (re-find #"[\d,X,Y]+" loc) band (re-find #"[p,q][\d+,\.]*" loc) not-empty? (complement empty?)]
		;(owl/owl-class
			;(str (if (not-empty? chrom) (str "HumanChromosome" chrom))  (if (not-empty? band) (str "Band" band)))
			;:subclass (str "HumanChromosome" chrom)
		;)
	;)
;)

(defn loc-parse [loc]
	(let [chrom (re-find #"[\d,X,Y]+" loc) band (re-find #"[p,q][\d+,\.,ter]*" loc) not-empty? (complement empty?)]
		(owl-class
			(str (if (not-empty? chrom) (str "HumanChromosome" chrom))  (if (not-empty? band) (str "Band" band)))
			:super (owl-some pb/IsLocatedOn (owl-class (str "HumanChromosome" chrom)))
		)
	)
)

(defn loc-parse-with-qter [loc chrom]
	(let [chrom (re-find #"[\d,X,Y]+" loc) band (re-find #"[p,q][\d+,\.,ter]*" loc) not-empty? (complement empty?)]
		(owl-class	
			(str (if (not-empty? chrom) (str "HumanChromosome" chrom))  (if (not-empty? band) (str "Band" band)) "!qter")
			:super (owl-some pb/IsLocatedOn (owl-class (str "HumanChromosome" chrom)))
		)
	)
)

;(defn Receivepart [receive chrom]
	;(owl-some pb/HasReceivePart (loc-parse-with-qter receive chrom))
;)

;;incomplete with bugs
;(defn exchange-parse [coll]
	;(let [receive (first coll) provide (first (rest coll))
		  ;receive-chrom (re-find #"[X Y \d]*(?=[p q])" receive)
		  ;provide-chrom (re-find #"[X Y \d]*(?=[p q])" provide)
		  ;coll (vector receive provide-chrom provide receive-chrom)
		  ;create (fn [part chrom] (owl-some pb/HasTranslocation (loc-parse-with-qter part chrom)))
		  ;]
	;(map create list (rest list))
	;)
;)

(defn Tral-provide-parse [coll]
	(let [receive (first coll) provide (first (rest coll))
		  receive-chrom (re-find #"[X Y \d]*(?=[p q])" receive)
		  provide-chrom (re-find #"[X Y \d]*(?=[p q])" provide)]
		  (owl-some pb/HasTranslocation (loc-parse-with-qter receive provide-chrom))
	)
)

(defn Tral-receive-parse [coll]
	(let [receive (first coll) provide (first (rest coll))
		  receive-chrom (re-find #"[X Y \d]*(?=[p q])" receive)
		  provide-chrom (re-find #"[X Y \d]*(?=[p q])" provide)]
		  (owl-some pb/HasTranslocation (loc-parse-with-qter provide receive-chrom))
	)
)






;parse functions
;check for "+",return details like ("+3" "+7").
(defn Plus [karyotype]
	(re-seq #"\+[\d X Y]*" karyotype))

;check for "-",return details like ("-3" "-7").
(defn Minus [karyotype]
	(re-seq #"(?<=\,)\-[\d X Y]*" karyotype))

;check for Fail.
(defn Fail? [karyotype]
	(if (re-find #"Fail" karyotype)
		true
		false
	))

;check for Idem (stemline karyotype in a subclone).
(defn Idem? [karyotype]
	(if (re-find #"idem" karyotype)
		true
		false
	))


;check for +mar (mark chromosome).
(defn Mar? [karyotype]
	(if (re-find #"mar" karyotype)
		true
		false
	))

;check for inc (incomplete karyotype).
(defn Inc? [karyotype]
	(if (re-find #"inc" karyotype)
		true
		false
	))

;Dash

;check for del.
(defn Del [karyotype]
	(let [sub (re-seq #"del[\d,p,q,\(,\),\.]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for add.
(defn Add [karyotype]
	(let [sub (re-seq #"add[\d,p,q,\(,\),\.]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for dup.
(defn Dup [karyotype]
	(let [sub (re-seq #"dup[\d,p,q,\(,\),\.]*\)" karyotype)] 
		(->> sub
			;convert "dup" to "duP" in strings to avoid misunderstanding because of "p".
			(map (fn [expr] (str/replace expr #"up" "uP")))
			(map Extr-loc)
		)
	)	
)

;check for trp.
(defn Trp [karyotype]
	(let [sub (re-seq #"trp[\d,p,q,\(,\),\.]*\)" karyotype)] 
		(->> sub
			;convert "trp" to "trP" in strings to avoid misunderstanding because of "p".
			(map (fn [expr] (str/replace expr #"rp" "rP")))
			(map Extr-loc)
		)
	)	
)

;check for i.
(defn Iso [karyotype]
	(let [sub (re-seq #"i[\d,p,q,\(,\),\.]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for t.
(defn Tral-a [karyotype]
	(let [firpa (fn [exp] (re-seq #"(?<=[\; \(])[\d]*" exp))
		secpa (fn [exp] (re-seq #"[p q][\d]*" exp))]
		(map str/join (map list (firpa karyotype) (secpa karyotype)))))

(defn Tral [karyotype]
	(let [sub (re-seq #"(?<=,)t[\d,p,q,\(,\),\.,\;]*\)" karyotype)] 
		(first (into [] (map Extr-mult-loc sub)))
	)
)

;check for inv.
(defn Inv [karyotype]
	(let [sub (re-seq #"inv[\d,p,q,\(,\),\;,\.]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for der.(incomplete)
(defn Der? [karyotype]
	(if (re-find #"der" karyotype)
		true
		false
	))


;extract chromosome numbers.
(defn parse-int [string]
   (Integer. (re-find  #"\d+" string)))

(defn Chr-num [karyotype]
	(parse-int (first (str/split karyotype #","))))

(defn Chr-num-new [karyotype]
	(let [not-empty? (complement empty?)]
		(map read-string (filter not-empty? (re-seq  #"[0-9]*" (first (str/split karyotype #",")))))
	)
)

;;chromosome number of database varies between 44 and 67.


;option = delete ? or put it into re

;;file handle
(defn clean-karyotype [karyotype]
	(-> karyotype
		(str/replace "?" "")
		(str/replace #"\[[C,P,\d]*\]" "")
	)
)

(defn make-dic [line]
	(let [div (str/split line #"@@")] 
		(hash-map (first div) (clean-karyotype (get (into [] (rest div)) 0)))
	)
)

(defn make-database [loc]
	(apply merge
		(map make-dic
			(with-open [rdr (io/reader "D:/project/parser/karyotype-parser/test.txt")]
				(doall (line-seq rdr))))))


