(defpackage #:bnf
  (:use #:endaira)
  (:export #:bnf))
(in-package #:bnf)

(defvar *max* 1)

#| BNF
(bnf (name &key max)
     (&rest syntax)
     :where
     bind*)

name := symbol
max := integer
syntax := (var literal group)*
var := symbol
|#

#+design
(bnf(integer :max 4)
  (sign? digit+ dot?)
  :where
  (sign '(#\+ #\-))
  (digit (coerce "1234567890" 'list))
  (dot #\.))

#+design
(bnf(ratio :max 3)
  (sign? digit+ #\/ digit+)
  :where
  (sign '(#\+ #\-))
  (digit (coerce "1234567890" 'list)))

#+design
(bnf(float)
  #++(sign? digit* #\. digit+ exponent?)
  (or (sign? digit* #\. digit+ exponent?)
      (sign? digit+ (#\. digit*)? exponent))
  :where
  (sign '(#\+ #\-))
  (digit (coerce "1234567890" 'list))
  (exponent (bnf(exponent)
	      (marker sign? digit+)
	      :where
	      (marker (coerce "esfdlESFDL" 'list)))))

(defmacro bnf((name &key(max '*max*))
	      (&rest clause*)&optional(where :where) &rest bind*)
  (declare(ignore name))
  (assert(eq :where where))
  `(LET((*MAX* ,max))
     (LET*,(mapcar (lambda(bind)
		     `(,(car bind)(UIOP:ENSURE-LIST ,(cadr bind))))
		   bind*)
       ,@(parse clause*))))

#+example
(integrate-plist #'+ '(:a 1 :b 2 :c 3)'(:b 1 :c 2 :d 3))
; =>  (:D 3 :C 5 :B 3 :A 1) ; not stable.
(defun integrate-plist(function &rest plists)
  (loop :for plist :in plists
	:with hash-table = (make-hash-table :test #'eq)
	:do (loop :for (key value) :on plist :by #'cddr
		  :do (multiple-value-bind(v exist?)(gethash key hash-table)
			(if exist?
			  (setf (gethash key hash-table)(funcall function v value))
			  (setf (gethash key hash-table)value))))
	:finally (return (alexandria:hash-table-plist hash-table))))

;; In order to be able to evaluate variable, `PARSE` must return plist.
#+example (parse '(sign? digit+ dot?))
; =>
(defun parse(clause*)
  (check-type clause* list)
  (case (car clause*)
    ((and not)(error "not supported ~S"clause*))
    ((or) `((INTEGRATE-PLIST #'APPEND ,@(mapcan #'parse (cdr clause*)))))
    (otherwise `((%BNF,@(en-argument clause*))))))

;; In order to get randam value of sample, and to be able to refer variable,
;; `SAMPLE` must be closure.
#+example
(en-argument '(sign? digit+ dot*))
; => (:optional sign :require (lambda()(sample digit)) :optional (lambda()(sample dot)))
#+example
(en-argument '((sign digit+)?))
; => (:optional (%bnf :require sign :require (lambda()(sample digit))))
(defun en-argument(clause)
  (labels((rec(list &optional acc)
	    (if(endp (cdr list))
	      (do-return list acc)
	      (body (car list)(cdr list)acc)))
	  (body(token rest acc)
	    (when(listp token) ; as canonicalize.
	      (setf token (cons '%bnf (en-argument token))))
	    (multiple-value-bind(key multiple?)(keyp (car rest))
	      (if key
		(if multiple?
		  (rec (cdr rest)(list* `(lambda()(sample ,token)) key acc))
		  (rec (cdr rest)(list* token key acc)))
		(rec rest (acc token acc)))))
	  (keyp(token)
	    (case token
	      (? :optional)
	      (* (values :optional T))
	      (+ (values :require T))))
	  (acc(token acc)
	    (if(not(symbolp token))
	      (list* token :require acc)
	      (multiple-value-bind(key var multiple?)(split(symbol-name token)token)
		(if multiple?
		  (list* `(lambda()(sample ,var))key acc)
		  (list* var key acc)))))
	  (split(name symbol)
	    (let((position(1-(length name))))
	      (multiple-value-bind(key multiple?)(keyp(read-from-string name t t :start position))
		(if key
		  (if multiple?
		    (values key (read-from-string name t t :end position) T)
		    (values key(read-from-string name t t :end position)nil))
		  (values :require symbol nil)))))
	  (do-return(list acc)
	    (if list
	      (nreverse(acc(car list)acc))
	      (nreverse acc)))
	  )
    (rec clause)))

#+design
(%bnf :optional '(#\+ #\-)
      :require (lambda()(sample '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
      :optional '(#\.))
#| => (:valid ("123" "+123" "-123" "123." "+123." "-123.")
       :invalid ("" "+" "-" "+." "-."))      
|#
#+example
(%bnf :require '(:valid ("+") :invalid ("")))
#+example
(%bnf :optional '(#\+ #\-)
      :optional (lambda()(sample(coerce "1234567890" 'list)))
      :require #\.
      :require (lambda()(sample(coerce "1234567890" 'list)))
      :optional '(:valid ("e8" "e+8" "e-0") :invalid ("" "1" "+" "+8")))

(defun %bnf(&rest args)
  (multiple-value-bind(forms operators)(map-args args)
    (multiple-value-bind(valids invalids)(sieve (combinate forms)operators)
      (list :valid valids :invalid invalids))))

#+example
(map-args (list :optional '(#\+ #\-)
		:optional (lambda()(sample(coerce "1234567890" 'list)))
		:require #\.
		:require (lambda()(sample(coerce "1234567890" 'list)))
		:optional '(:valid ("e8" "e+8" "e-0") :invalid ("" "1" "+" "+8"))))
(defun map-args(args)
  (flet((marking(form)
	  (append (mapcar(lambda(x)(cons :valid x))(getf form :valid))
		  (mapcar(lambda(x)(cons :invalid x))(getf form :invalid)))))
    (loop :for (operator form) :on args :by #'cddr
	  :collect operator :into operators
	  :if(or (functionp form) ; Do not call function here! SIEVE do it.
		 (atom form))
	  :collect (list "" form) :into forms
	  :else :if (typep form '(CONS(EQL :VALID)T))
	  :collect (marking form) :into forms
	  :else :collect (cons "" form) :into forms
	  :finally (return (values forms operators)))))

#+example (combinate '((1 2))) ; => ((1)(2))
#+example (combinate '((1 2)(3 4))) ; => ((1 3)(1 4)(2 3)(2 4))
#+example (combinate '((1 2)(3 4)(5 6))) ; =>
#| ((1 3 5)(1 3 6)(1 4 5)(1 4 6)
    (2 3 5)(2 3 6)(2 4 5)(2 4 6))
|#
(defun combinate(forms)
  (mapcan (lambda(elt)
	    (if(cdr forms)
	      (mapcar (lambda(x)
			(cons elt x))
		      (combinate (cdr forms)))
	      (list (list elt))))
	  (car forms)))

;; In order to get random value from sample, SIEVE responds to evaluate SAMPLE.
#+example
(sieve '(("" "" "" "" "")
	 ("" "" "" "" (:invalid . "+8")))
       '(:optional :optional :require :require :optional))
(defun sieve(values operators)
  (flet((en-value(list &optional validp)
	  (loop :for elt :in list
		:when (functionp elt)
		  :collect (funcall elt) :into result
		:else :if(consp elt)
		  :collect (cdr elt) :into result
		:else :collect elt :into result
		:finally(return (uiop:reduce/strcat result)))))
    (loop :for value :in values
	  :if (validp value operators)
	  :collect (en-value value T) :into valids
	  :else :collect (en-value value) :into invalids
	  :finally (return(values valids invalids)))))

(defun validp (value operators)
  (loop :for v :in value
	:for o :in operators
	:never (or (and (eq :require o)
			(equal "" v))
		   (and (consp v)
			(eq :invalid (car v))))))

(defun sample(samples &key((:max *max*) *max*))
  (assert(not(minusp *max*)))
  (loop :with samples = (coerce samples 'vector)
	:with length = (length samples)
	:repeat *max*
	:collect (aref samples (random length)) :into result
	:finally (return (uiop:reduce/strcat result))))

(defun bnf-check(function result)
  (let((acc))
    (push :valid acc)
    (dolist(v(second result))
      (unless(funcall function v)
	(push v acc)))
    (push :invalid acc)
    (dolist(i(fourth result))
      (when(funcall function i)
	(push i acc)))
    (nreverse acc)))

(defun checker(type)
  (lambda(x)
    (typep (handler-case(read-from-string x)
	     (error(c)c))
	   type)))
