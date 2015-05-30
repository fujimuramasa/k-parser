(ns karyotype-parser.core
	(:require 
		[clojure.string :as str]
		)
  	(:gen-class))

;check for "+",return details like ("+3" "+7").
(defn Plus [karyotype]
	(re-seq #"\+[\d X Y]*" karyotype))

;check for "-",return details like ("-3" "-7").
(defn Minus [karyotype]
	(re-seq #"\-[\d X Y]*" karyotype))

;check for Fail.
(defn Fail? [karyotype]
	(if (re-find #"Fail" karyotype)
		'True
		'False
	))

;check for Idem (stemline karyotype in a subclone).
(defn Idem? [karyotype]
	(if (re-find #"idem" karyotype)
		'True
		'False
	))

;check for add (additional material of unkown origin).
;(defn Add? [karyotype]
	;(if (re-find #"add" karyotype)
		;'True
		;'False
	;))

;check for +mar (mark chromosome).
(defn Mar? [karyotype]
	(if (re-find #"\+mar" karyotype)
		'True
		'False
	))

;check for inc (incomplete karyotype).
(defn Inc? [karyotype]
	(if (re-find #"inc" karyotype)
		'True
		'False
	))

;Dash

(defn Extr-loc [exp]
	(str/join (re-seq #"[\d p q]" exp))
)

(defn Extr-mult-loc [exp]
	(let [not-empty? (complement empty?)
		coll (into [] (filter not-empty? (re-seq #"[\d,p,q]*" exp)))
		len (count coll)]
			(map (fn [x] (str/join [(nth coll x) (nth coll (+ x (/ len 2)))])) (range (/ len 2)))
	)
)

;check for del.
(defn Del [karyotype]
	(let [sub (re-seq #"del[\d p q \( \)]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for add.
(defn Add [karyotype]
	(let [sub (re-seq #"add[\d p q \( \)]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

;check for dup.
(defn Dup [karyotype]
	(let [sub (re-seq #"dup[\d p q \( \)]*\)" karyotype)] 
		(->> sub
			;convert "dup" to "duP" in strings to avoid misunderstanding because of "p".
			(map (fn [expr] (str/replace expr #"up" "uP")))
			(map Extr-loc)
		)
	)	
)

;check for trp.
(defn Trp [karyotype]
	(let [sub (re-seq #"trp[\d p q \( \)]*\)" karyotype)] 
		(->> sub
			;convert "trp" to "trP" in strings to avoid misunderstanding because of "p".
			(map (fn [expr] (str/replace expr #"rp" "rP")))
			(map Extr-loc)
		)
	)	
)

;check for i.
(defn Iso [karyotype]
	(let [sub (re-seq #"i[\d p q \( \)]*\)" karyotype)] 
		(map Extr-loc sub)
	)
)

