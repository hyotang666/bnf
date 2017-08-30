(defpackage #:bnf
  (:use #:endaira)
  (:export #:bnf))
(in-package #:bnf)

;;; Original bnf notation example.
#| 
integer := sign digits dot
sign := [ #\+ | #\- | "" ]
digits := [ digit | digit digits ]
digit := [ #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 | #\0 ]
dot := [ #\. | "" ]
|#

#+design-of-expanded-form
(labels((dot()
	  (funcall (aref (vector (constantly #\.)(constantly ""))
			 (random 2))))
	(digit()
	  (funcall (aref (map 'vector #'constantly "1234567890")
			 (random 10))))
	(sign()
	  (funcall (aref (vector (constantly #\+)(constantly #\-)(constantly ""))
			 (random 3))))
	(digits(&optional (max 3))
	  (if(zerop max)
	    ""
	    (funcall (callconcer #'digit (case(random 2)
					   (0 #'digit)
					   (1 (lambda()(digits(1- max)))))))))
	(integer()
	  (funcall(callconcer #'sign #'digits #'dot)))
	)
  (integer))

#+design-of-syntax
(genbnf (integer (sign digits dot))
	(sign (or "" #\+ #\-))
	(digits (or digit (digit digits))
		:max 3)
	(digit (or . #.(coerce "1234567890" 'list)))
	(dot (or "" #\.)))
#+design-of-syntax
(genbnf (ratio (sign digits #\/ digits))
	(sign (or "" "+" "-"))
	(digit (or . #.(coerce "1234567890" 'list)))
	(digits (or digit (digit digits))
		:max 3))

(defun callconcer(&rest functions)
  (constantly (uiop:reduce/strcat(mapcar #'funcall functions))))

(defmacro genbnf(&rest clause*)
  `(labels,(mapcar #'genbnf-clause clause*)
     (,(caar clause*))))

(defun genbnf-clause(clause)
  (destructuring-bind(var form &key max)clause
    `(,var,(when max `(&optional(max ,max)))
       ,(if max
	  `(if(zerop max)
	     ""
	     (funcall,(genbnf-parse form var)))
	  `(funcall ,(genbnf-parse form var))))))

(defun or-form(form var)
  `(aref (vector ,@(mapcar (lambda(elt)
			     (genbnf-parse elt var))
			   (cdr form)))
	 (random ,(length (cdr form)))))

(defun genbnf-parse (form var)
  (typecase form
    ((or character string)`(constantly ,form))
    (symbol (if(eq form var)
	      `(lambda()(,form (1- max)))
	      `(function ,form)))
    ((CONS (EQL OR) T)
     (or-form form var))
    (otherwise `(callconcer ,(genbnf-parse(car form) var) 
			    (case(random ,(length form))
			      ,@(loop :for i :upfrom 0
				      :for f :in form
				      :collect `(,i ,(genbnf-parse f var))))))))

;;; Original bnf notation example.
#| 
integer := sign digits dot
sign := [ #\+ | #\- | "" ]
digits := [ digit | digit digits ]
digit := [ #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 | #\0 ]
dot := [ #\. | "" ]
|#

#+design
(labels((integer()
	  (list (sign)(digits)(dot)))
	(sign()
	  '("" #\+ #\-))
	(digits()
	  (labels((digits(&optional(max 3))
		    (if(zerop max)
		      ""
		      (funcall (aref (vector (lambda()(sample(digit)))
					     (lambda()(uiop:strcat (sample(digit))
								   (digits(1- max)))))
				     (random 2))))))
	    (list #'digits)))
	(digit()
	  '#.(coerce "1234567890" 'list))
	(dot()
	  '("" #\.))
	)
  (mapcar #'strcat (combinate (integer))))

#+design-of-syntax
(bnf (integer (sign digits dot))
     (sign (or "" #\+ #\-))
     (digits (or digit (digit digits)) :max 3)
     (digit (or . #.(coerce "1234567890" 'list)))
     (dot (or "" #\.)))

(defmacro bnf(&rest clause*)
  (case (length clause*)
    (0 nil)
    (1 `(LABELS,(mapcar #'bnf-clause clause*)
	  (MAPCAR #'STRCAT (COMBINATE(MAPCAR #'UIOP:ENSURE-LIST(CANONICALIZE ,(if(typep (car clause*) '(CONS (EQL OR) T))
										T
										NIL)
									     (,(caar clause*))))))))
    (otherwise `(LABELS,(mapcar #'bnf-clause clause*)
		  (LET((LIST(,(caar clause*))))
		    (IF(EQ :EXAMPLES (CAR LIST))
		      (CDR LIST)
		     LIST))))))

(defun bnf-clause(clause)
  (destructuring-bind(var form &key max)clause
    (if max
      `(,var ()(LABELS((,var(&optional (MAX (1+(RANDOM ,max))))
			 (IF(<= MAX 0)
			   ""
			   ,(labels-parse form var))))
		 (LIST #',var)))
      `(,var()
	 (RETURN-FROM ; <--- In order to return string. Otherwise treated as docstring.
	   ,var ,(bnf-parse form))))))

(defun labels-parse(form var)
  (labels((parse(form)
	    (typecase form
	      ((or character string)form)
	      (symbol (if (eq form var)
			`(,form(1- max))
			`(sample (,form))))
	      ((cons(eql or)t)(or-form form))
	      (otherwise `(uiop:strcat ,@(mapcar #'parse form)))))
	  (or-form(form)
	    `(FUNCALL(AREF (VECTOR ,@(mapcar (lambda(elt)
					       `(LAMBDA(),(parse elt)))
					     (cdr form)))
			   (RANDOM ,(length (cdr form)))))))
    (if(typep form '(cons (eql or)t))
      (or-form form)
      `(uiop:strcat ,(parse form)(,var(1- max))))))

(defun bnf-parse(form)
  (typecase form
    ((or character string)form) ; <--- literal data.
    (symbol`(,form)) ; <--- function call.
    ((cons(eql or)T)
     `(CANONICALIZE T ,@(mapcar #'bnf-parse (cdr form))))
    (otherwise `(CONS :EXAMPLES(MAPCAR #'STRCAT (COMBINATE(MAPCAR #'UIOP:ENSURE-LIST(CANONICALIZE NIL ,@(mapcar #'bnf-parse form)))))))))

(defun canonicalize(orp &rest elt*)
  (loop :for elt :in elt*
	:when (typep elt '(cons (eql :examples) t))
	  :if orp
	  :append (cdr elt)
	  :else :collect (cdr elt)
	:else :collect elt))

(defun sample(list)
  (aref(coerce list 'vector)(random (length list))))

(defun combinate(forms)
  (mapcan (lambda(elt)
	    (if(cdr forms)
	      (mapcar (lambda(x)
			(cons elt x))
		      (combinate (cdr forms)))
	      (list (list elt))))
	  (car forms)))

(defun strcat(list)
  (loop :for elt :in list
	:collect (typecase elt
		   (function (funcall elt))
		   (list (strcat elt))
		   (otherwise elt))
	:into result
	:finally (return(uiop:reduce/strcat result))))

#+design-of-syntax
(bnf (integer (sign digits dot))
     (sign (or "" #\+ #\-))
     (digits (or digit (digit digits)) :max 3)
     (digit (or . #.(coerce "1234567890" 'list)))
     (dot (or "" #\.)))

#+design-of-syntax
(bnf (ratio (sign digits #\/ digits))
     (sign (or "" "+" "-"))
     (digit (or . #.(coerce "1234567890" 'list)))
     (digits (or digit (digit digits))
	     :max 3))

