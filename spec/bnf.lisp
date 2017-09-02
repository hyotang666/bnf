(defpackage :bnf.spec
  (:import-from :bnf #:combinate #:strcat #:canonicalize)
  (:shadowing-import-from :endaira #:labels)
  (:use :cl :jingoh #:bnf))
(in-package :bnf.spec)
(setup :bnf)

(requirements-about EXAMPLES)

;;;; Description:
; Generate all inclusive notation examples of specified bnf.

#+syntax
(EXAMPLES &rest clause*) ; => result

#?(examples (integer (sign? digit+ dot?))
	    (sign? (or "" #\+ #\-))
	    (digit+ (or digit (digit digit+)):max 3)
	    (digit (or . #.(coerce "1234567890" 'list)))
	    (dot? (or "" #\.)))
=> ("4" "1." "+138" "+458." "-6" "-101.")
,:test (lambda(example $result)
	 (declare(ignore example))
	 (& (listp $result)
	    (= (length $result)
	       #.(* 3 ; sign?
		    1 ; digit+
		    2) ; dot?
	       )
	    (every #'stringp $result)
	    (every (lambda(elt)
		     (integerp(read-from-string elt)))
		   $result)))

;;;; Arguments and Values:

; clause := (name definition &key max)

; name := symbol, otherwise error.
#?(examples("not-symbol" #\.)) :signals error

; It expanded to local function name internally.
#?(examples(sign (or #\+ #\-)))
:expanded-to 
(labels((sign()
	  (return-from sign (canonicalize t #\+ #\-))))
  (mapcar #'strcat (combinate (mapcar #'uiop:ensure-list (canonicalize nil (sign))))))

; TODO
; Unlock package.
; Currently violate or not is implementation dependent.
#?(examples(float "float")) => unspecified

; definition := [ name | choices | literal | group ]

; choices := (or definition*)
#?(examples(sign (or #\+ #\-)))
=> ("+" "-")
,:test equal

#?(examples(empty(or))) => NIL

#?(examples (works? (empty empty))
	    (empty (or)))
=> NIL

; literal := [ character | string ]
#?(examples(alpha #\a)) => ("a")
,:test equal

#?(examples(my-name "sato")) => ("sato")
,:test equal

; group := (definition+)
#?(examples (http (#\h "tt" #\p)))
=> ("http")
,:test equal

; When group is null, an error is signaled.
#?(examples (http ())) :signals error

; max := non-negative-integer
; Specify max recursive depth.

; Case literal character.
#?(examples(alpha #\a :max 4))
:satisfies #`(find $result '(("a")("aa")("aaa")("aaaa")):test #'equal)

; Case literal string.
#?(examples(my-name "sato" :max 4))
:satisfies #`(find $result '(("sato")
			     ("satosato")
			     ("satosatosato")
			     ("satosatosatosato"))
		   :test #'equal)

; Case group.
#?(examples(http (#\h "tt" #\p) :max 4))
:satisfies #`(find $result '(("http")
			     ("httphttp")
			     ("httphttphttp")
			     ("httphttphttphttp"))
		   :test #'equal)

; Case choices.
#?(examples(sign2 (or #\+ #\-) :max 2))
:satisfies #`(find $result '(("+")("-")("++")("+-")("--")("-+"))
		   :test #'equal)

#?(examples (zeros (or #\0 (#\0 zeros))
		   :max 3))
:satisfies #`(find $result '(("0")("00")("000"))
		   :test #'equal)

; Case name.
#?(examples (zeros zero :max 3)
	    (zero #\0))
:satisfies #`(find $result '(("0")("00")("000"))
		   :test #'equal)

#?(examples (zeros (or zero (zero zeros))
		   :max 3)
	    (zero #\0))
:satisfies #`(find $result '(("0")("00")("000"))
		   :test #'equal)

; result := list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Could not refer any outer scope.
#?(let((signs '(or #\+ #\-)))
    (examples (sign? (or "" signs))))
:signals error
,:ignore-signals warning

; The definition at first is treated as special.
; I.e. It becomes entry point.
;;;; Exceptional-Situations:

;;;; Examples:
#?(examples (ratio (sign digits #\/ digits))
	    (sign (or "" "+" "-"))
	    (digit (or . #.(coerce "1234567890" 'list)))
	    (digits (or digit (digit digits))
		    :max 3))
=> #|e.g.|# ("58/35" "+3/3" "-2/2")
,:test (lambda(e.g. result)
	 (declare(ignore e.g.))
	 (& (listp result)
	    (= (length result)
	       #.(* 3 ; sign
		    1 ; digits
		    1 ; #\/
		    1 ; digits
		    ))
	    (every #'stringp result)
	    (every (lambda(elt)
		     (rationalp(read-from-string elt)))
		   result)))

#?(examples (float. (or (sign? digit* #\. digit+ (or "" exponent))
			(sign? digit+ (or "" (#\. digit*)) exponent)))
	    (sign? (or "" #\+ #\-))
	    (digit* (or "" digit+))
	    (digit+ (or . #.(coerce "1234567890" 'list))
		    :max 1)
	    (exponent (marker sign? digit+))
	    (marker (or . #.(coerce "DEFLSdefls" 'list))))

:satisfies #`(& (listp $result)
		(every #'stringp $result)
		(= (length $result)
		   #.(+ (* 3 ; sign?
			   2 ; digit*
			   1 ; #\.
			   1 ; digit+
			   (+ 1 ; ""
			      (* 10 ; marker
				 3 ; sign?
				 1 ; digit+
				 )))
			(* 3 ; sign?
			   1 ; digit+
			   (+ 1 ; ""
			      (* 1 ; #\.
				 2 ; digit*
				 ))
			   (* 10 ; marker
			      3 ; sign?
			      1 ; digit+
			      ))))
		(every (lambda(elt)
			 (floatp(read-from-string elt)))
		       $result))
