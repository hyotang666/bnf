(defpackage :bnf.spec
  (:import-from :bnf #:combinate #:strcat #:canonicalize)
  (:shadowing-import-from :endaira #:labels)
  (:use :cl :jingoh #:bnf))
(in-package :bnf.spec)
(setup :bnf)

(requirements-about BNF)

;;;; Description:
; Generate all inclusive notation examples of specified bnf.

#+syntax
(BNF &rest clause*) ; => result

#?(bnf (integer (sign? digit+ dot?))
       (sign? (or "" #\+ #\-))
       (digit+ (or digit (digit digit+)):max 3)
       (digit (or . #.(coerce "1234567890" 'list)))
       (dot? (or "" #\.)))
=> ("4" "1." "+138" "+458." "-6" "-101.")
,:test (lambda(example $result)
	 (declare(ignore example))
	 (& (listp $result)
	    (every #'stringp $result)
	    (every (lambda(elt)
		     (integerp(read-from-string elt)))
		   $result)))

;;;; Arguments and Values:

; clause := (name definition &key max)

; name := symbol, otherwise error.
#?(bnf("not-symbol" #\.)) :signals error

; It expanded to local function name internally.
#?(bnf(sign (or #\+ #\-)))
:expanded-to 
(labels((sign()
	  (return-from sign (canonicalize t #\+ #\-))))
  (mapcar #'strcat (combinate (mapcar #'uiop:ensure-list (canonicalize nil (sign))))))

; TODO
; Unlock package.
; Currently violate or not is implementation dependent.
#?(bnf(float "float")) => unspecified

; definition := [ name | choices | literal | group ]

; choices := (or definition*)
#?(bnf(sign (or #\+ #\-)))
=> ("+" "-")
,:test equal

#?(bnf(empty(or))) => NIL

#?(bnf (works? (empty empty))
       (empty (or)))
=> NIL

; literal := [ character | string ]
#?(bnf(alpha #\a)) => ("a")
,:test equal

#?(bnf(my-name "sato")) => ("sato")
,:test equal

; group := (definition+)
#?(bnf (http (#\h "tt" #\p)))
=> ("http")
,:test equal

; When group is null, an error is signaled.
#?(bnf (http ())) :signals error

; max := non-negative-integer
; Specify max recursive depth.

; Case literal character.
#?(bnf(alpha #\a :max 4))
:satisfies #`(find $result '(("a")("aa")("aaa")("aaaa")):test #'equal)

; Case literal string.
#?(bnf(my-name "sato" :max 4))
:satisfies #`(find $result '(("sato")
			     ("satosato")
			     ("satosatosato")
			     ("satosatosatosato"))
		   :test #'equal)

; Case group.
#?(bnf(http (#\h "tt" #\p) :max 4))
:satisfies #`(find $result '(("http")
			     ("httphttp")
			     ("httphttphttp")
			     ("httphttphttphttp"))
		   :test #'equal)

; Case choices.
#?(bnf(sign2 (or #\+ #\-) :max 2))
:satisfies #`(find $result '(("+")("-")("++")("+-")("--")("-+"))
		   :test #'equal)

; result := list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Could not refer any outer scope.
#?(let((signs '(or #\+ #\-)))
    (bnf (sign? (or "" signs))))
:signals error
,:ignore-signals warning

; The definition at first is treated as special.
; I.e. It becomes entry point.
;;;; Exceptional-Situations:

;;;; Examples:
#?(bnf (ratio (sign digits #\/ digits))
       (sign (or "" "+" "-"))
       (digit (or . #.(coerce "1234567890" 'list)))
       (digits (or digit (digit digits))
	       :max 3))
=> #|e.g.|# ("58/35" "+3/3" "-2/2")
,:test (lambda(e.g. result)
	 (declare(ignore e.g.))
	 (& (listp result)
	    (every #'stringp result)
	    (every (lambda(elt)
		     (rationalp(read-from-string elt)))
		   result)))

#?(bnf (float. (or (sign? digit* #\. digit+ (or "" exponent))
		   (sign? digit+ (or "" (#\. digit*)) exponent)))
       (sign? (or "" #\+ #\-))
       (digit* (or "" digit+))
       (digit+ (or . #.(coerce "1234567890" 'list))
	       :max 2)
       (exponent (marker sign? digit+))
       (marker (or . #.(coerce "DEFLSdefls" 'list))))

:satisfies #`(& (listp $result)
		(every #'stringp $result)
		(every (lambda(elt)
			 (floatp(read-from-string elt)))
		       $result))
