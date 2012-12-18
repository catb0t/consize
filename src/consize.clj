;;; Consize -- A concatenative programming language (when size matters)
;;; Copyright (c) 2012, Dominikus Herzberg, Heilbronn University, Germany
;;; New BSD License: http://opensource.org/licenses/BSD-3-Clause
(ns
	^{:doc "Consize -- A concatenative programming language (when size matters)"
	  :author "Dominikus Herzberg, Heilbronn University, Germany" }
  consize
  (:use [clojure.string :only (lower-case split trim)]))

;; Words for stack shuffling
(defn Swap [y x & r] (conj r y x))
(defn Dup  [x & r] (conj r x x))
(defn Drop [x & r] (sequence r))
(defn Rot  [z y x & r] (conj r y z x))

;; Words for type and equality
(defn Type [itm & r]
	(cond (string? itm) (conj r "wrd")
		    (seq?    itm) (conj r "stk")
		    (map?    itm) (conj r "map")
		    (fn?     itm) (conj r "fct")
		    (nil?    itm) (conj r "nil") ; experimental
				:else         (conj r "_|_"))) ; Exit because of internal error

(defn	Equal? [x y & r] (conj r (if (= x y) "t" "f")))
(defn Identical? [x y & r] (conj r (if (identical? x y) "t" "f"))) ; optional

;; Words for stacks
(defn Emptystack [& r] (conj r ()))
(defn Push [x s & r] {:pre [(seq? s)]} (conj r (conj s x)))
(defn Top [s & r] {:pre [(or (seq? s) (nil? s))]} (conj r (first s))) 
(defn Pop [s & r] {:pre [(or (seq? s) (nil? s))]} (conj r (rest s)))
(defn Concat [s2 s1 & r] {:pre [(seq? s1) (seq? s2)]} (conj r (concat s1 s2)))
(defn Reverse [s & r] {:pre [(seq? s)]} (conj r (reverse s)))

;; Words for maps; highly redundant for performance reasons
(defn Mapping [s & r] {:pre [(seq? s)]} (conj r (apply hash-map s)))
(defn Unmap [m & r] {:pre [(map? m)]}
	(conj r (sequence (reduce concat (seq m)))))
(defn Keys [m & r] {:pre [(map? m)]} (conj r (map identity (keys m))))
(defn Assoc [m k v & r] {:pre [(map? m)]} (conj r (assoc m k v)))
(defn Dissoc [m k & r] {:pre [(map? m)]} (conj r (dissoc m k)))
(defn Get [d m k & r] {:pre [(map? m)]} (conj r (get m k d)))
(defn Merge [m1 m2 & r] {:pre [(map? m1) (map? m2)]} (conj r (merge m2 m1)))

;; Words for Words
(defn- wordstack? [s] (and (not (empty? s)) (seq? s) (every? #(string? %) s)))	
(defn Word [s & r] {:pre [(wordstack? s)]} (conj r (reduce str s)))
(defn Unword [w & r] {:pre [w (string? w)]} (conj r (map str (seq w))))
(defn Char [w & r] {:pre [(string? w) (char? (read-string w))]}
	(conj r (str (read-string w))))

;; Words for I/O (console i.e. stdin/stdout)
(defn Print [w & r] {:pre [(string? w)]} (do (print w) (sequence r)))
(defn Flush [& r] (do (flush) (sequence r)))
(defn Read-line [& r] (conj r (read-line))) ; what about exceptions?

;; Words for I/O (files etc.)
(defn Slurp [w & r] {:pre [(string? w)]} (conj r (slurp w)))
(defn Spit [file data & r] {:pre [(string? file) (string? data)]}
  (do (spit file data) (sequence r)))
(defn Spit-on [file data & r] {:pre [(string? file) (string? data)]}
  (do (spit file data :append true) (sequence r)))

;; Words supporting parsing
(defn Uncomment [w & r] {:pre [(string? w)]}
  (conj r (reduce str (interpose "\r\n" (split w #"\s*%.*[(\r\n)\r\n]")))))
(defn Tokenize [w & r] {:pre [(string? w)]}
	(let [s (seq (split (trim w) #"\s+"))] (conj r (if (= s '("")) () s))))
(defn Undocument [w & r] {:pre [(string? w)]}
	(conj r (reduce str (interpose "\r\n"
    (map second (re-seq #"[(\r\n)\r\n]%?>> (.*)[(\r\n)\r\n]" w))))))

;; OS (http://docs.oracle.com/javase/1.4.2/docs/api/java/lang/System.html)
(defn Current-time-millis [& r] (conj r (str (System/currentTimeMillis))))
(defn Operating-system [& r] (conj r (str (System/getProperty "os.name"))))

;; The Interpreter being the Virtual Machine (VM)
(defn Stepcc [cs ds dict & r]
	{:pre [(seq cs) (seq? ds) (map? dict)]} ; (seq cs) tests if cs is not empty
	(let [itm  (first cs) rcs  (rest cs)
		    head (first ds) tail (rest ds)]
		(cond
			(string? itm)
			  (let [res (dict itm nil)]
				  (cond
					  (seq? res) (conj r dict ds (concat res rcs)) ; concatenation
					  (fn?  res) (conj r dict (apply res ds) rcs)  ; application
					  :else (case itm "call/cc"  (conj r dict (conj () tail rcs) head)
					                  "continue" (conj r dict (second ds) (first ds))
					                  "get-dict" (conj r dict (conj ds dict) rcs)
					                  "set-dict" (conj r head tail rcs)
					                  (conj r dict (conj ds itm) (conj rcs "read-word")))))
			(map? itm) (conj r dict (conj ds itm) (conj rcs "read-mapping"))
			:else (conj r dict (conj ds itm) rcs))))

(defn- runcc [cs ds dict]
	(if (empty? cs)
		ds
		(let [[cs' ds' dict']
			(try
				(Stepcc cs ds dict)
			  (catch Error e (list '("printer" "repl") (list "_|_" cs ds) dict))
			  (catch Exception e (list '("printer" "repl") (list "_|_" cs ds) dict)))]
			(recur cs' ds' dict'))))

;; Words for Functions
(defn Apply [f s & r] {:pre [(fn? f) (seq? s)]} (conj r (apply f s)))
(defn Compose [f2 f1 & r] {:pre [(fn? f1) (fn? f2)]}
	(conj r (fn [& ds] (apply f2 (apply f1 ds)))))
(defn Func [dict qt & r] {:pre [(map? dict) (seq? qt)]} ; function constructor
	(conj r (fn [& ds] (runcc qt (sequence ds) dict))))
    
(defn rooting [nsp] ; build root dictionary by reflection
  (apply merge
    (map (fn [sym] {(lower-case (name sym)) (eval sym)})
      (filter #(re-matches #"[A-Z<].*" (name %)) (keys (ns-publics nsp))))))

(defn- binary [op]
  (fn [y x & r] {:pre [(integer? (read-string x)) (integer? (read-string y))]}
  	(conj r (str (op (read-string x) (read-string y))))))
(defn- pred [op]
	(fn [y x & r] {:pre [(integer? (read-string x)) (integer? (read-string y))]}
		(conj r (if (op (read-string x) (read-string y)) "t" "f"))))
(defn Integer? [w & r]
	(conj r (if (string? w) (if (integer? (read-string w)) "t" "f") "f")))

(def root-dict (merge (rooting 'consize) ; 'user
  { "\\" (list (list "dup" "top" "rot" "swap" "push" "swap" "pop"
  	"continue") "call/cc"),
    "+" (binary +), "-" (binary -), "*" (binary *),
    "div" (binary quot), "mod" (binary mod),
    "<" (pred <), ">" (pred >), "==" (pred ==), "<=" (pred <=), ">=" (pred >=),
  }))

(defn -main []
	(Apply (first (Func root-dict
				   (first (apply Tokenize (apply Uncomment (Slurp "prelude.txt"))))))
		     ()))
(-main)
