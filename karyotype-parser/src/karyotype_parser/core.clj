(ns karyotype-parser.core
	(:require 
		[clojure.string :as str]
		[clojure.java.io :as io]
		[tawny.owl :as owl]
		)
  	(:gen-class))

;check for "+",return details like ("+3" "+7").
(defn Plus [karyotype]
	(re-seq #"\+[\d,X,Y]*" karyotype))

;check for "-",return details like ("-3" "-7").
(defn Minus [karyotype]
	(re-seq #"\-[\d,X,Y]*" karyotype))

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

;check for add (additional material of unkown origin).
;(defn Add? [karyotype]
	;(if (re-find #"add" karyotype)
		;'True
		;'False
	;))

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

(defn Extr-loc [exp]
	(str/join (re-seq #"[\d,p,q,\.]" exp))
)

(defn Extr-mult-loc [exp]
	(let [not-empty? (complement empty?)
		coll (into [] (filter not-empty? (re-seq #"[\d,p,q,\.]*" exp)))
		len (count coll)]
			(map (fn [x] (str/join [(nth coll x) (nth coll (+ x (/ len 2)))])) (range (/ len 2)))
	)
)

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
(defn Tral [karyotype]
	(let [sub (re-seq #"t[\d,p,q,\(,\),\.,\;]*\)" karyotype)] 
		(map Extr-mult-loc sub)
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
			(with-open [rdr (io/reader "D:/project/parser/karyotype-parser/original.txt")]
				(doall (line-seq rdr))))))


