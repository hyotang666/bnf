(defpackage #:bnf
  (:use #:cl)
  (:export #:examples))

(in-package #:bnf)

;;; Original bnf notation example.
#|
integer := sign digits dot
sign := [ #\+ | #\- | "" ]
digits := [ digit | digit digits ]
digit := [ #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 | #\0 ]
dot := [ #\. | "" ]
|#

(defun generate-digit-char ()
  (char "0123456789abcdefghijklmnopqrstuvwxyz" (random *read-base*)))

#+design
(labels ((integer ()
           (list (sign) (digits) (dot)))
         (sign ()
           '("" #\+ #\-))
         (digits ()
           (labels ((digits (&optional (max 3))
                      (if (zerop max)
                          ""
                          (funcall
                            (aref
                              (vector (lambda () (sample (digit)))
                                      (lambda ()
                                        (uiop:strcat (sample (digit))
                                                     (digits (1- max)))))
                              (random 2))))))
             (list #'digits)))
         (digit ()
           (generate-digit-char))
         (dot ()
           '("" #\.)))
  (mapcar #'strcat (combinate (integer))))

#+design-of-syntax
(examples
  (integer (sign digits dot))
  (sign (or "" #\+ #\-))
  (digits (or digit (digit digits)) :max 3)
  (digit #'generate-digit-char)
  (dot (or "" #\.)))

(defmacro examples (&body clause*)
  (case (length clause*)
    (0 nil)
    (1
     `(labels ,(mapcar #'<labels-def> clause*)
        (mapcar #'strcat
                (combinate
                  (mapcar #'uiop:ensure-list
                          (canonicalize
                            ,(if (typep (car clause*) '(cons (eql or) t))
                                 t
                                 nil)
                            (,(caar clause*))))))))
    (otherwise
     `(labels ,(mapcar #'<labels-def> clause*)
        (let ((list (,(caar clause*))))
          (typecase list
            ((cons (eql :examples) t) (cdr list))
            ((cons function null) (list (string (funcall (car list)))))
            (otherwise list)))))))

(defun <labels-def> (clause)
  (destructuring-bind
      (var form &key max)
      clause
    (if max
        `(,var nil
          (labels ((,var (&optional (max (1+ (random ,max))))
                     (if (<= max 0)
                         ""
                         ,(<with-max-form> form var))))
            (list #',var)))
        `(,var nil
          (return-from ; <--- In order to return string. Otherwise treated as
                       ; docstring.
            ,var
            ,(<simple-def-form> form))))))

(defun <with-max-form> (form var)
  (labels ((parse (form)
             (typecase form
               ((or character string) form)
               (symbol
                (if (eq form var)
                    `(,form (1- max))
                    `(sample (,form))))
               ((cons (eql or) t) (or-form form))
               (otherwise `(uiop:strcat ,@(mapcar #'parse form)))))
           (or-form (form)
             (let ((form
                    `(funcall
                       (aref
                         (vector
                           ,@(mapcar (lambda (elt) `(lambda () ,(parse elt)))
                                     (cdr form)))
                         (random ,(length (cdr form)))))))
               (if (trestrul:find-leaf var form)
                   form
                   `(uiop:strcat ,form (,var (1- max)))))))
    (if (typep form '(cons (eql or) t))
        (or-form form)
        `(uiop:strcat ,(parse form) (,var (1- max))))))

(defun <simple-def-form> (form)
  (typecase form
    ((or character string) form) ; <--- literal data.
    (symbol `(,form)) ; <--- function call.
    ((cons (eql or) t)
     `(canonicalize t ,@(mapcar #'<simple-def-form> (cdr form))))
    ((cons (eql function)) `(list ,(cdr form)))
    (otherwise
     `(cons :examples (mapcar #'strcat
                              (combinate
                                (mapcar #'uiop:ensure-list
                                        (canonicalize nil
                                                      ,@(mapcar
                                                          #'<simple-def-form>
                                                          form)))))))))

(defun canonicalize (orp &rest elt*)
  (loop :for elt :in elt*
        :when (typep elt '(cons (eql :examples) t))
          :if orp
            :append (cdr elt)
          :else
            :collect (cdr elt)
        :else
          :collect elt))

(defun sample (list)
  (if (listp list)
      (aref (coerce list 'vector) (random (length list)))
      list))

(defun combinate (forms)
  (mapcan
    (lambda (elt)
      (if (cdr forms)
          (mapcar (lambda (x) (cons elt x)) (combinate (cdr forms)))
          (list (list elt))))
    (car forms)))

(defun strcat (list)
  (loop :for elt :in list
        :collect (typecase elt
                   (function (funcall elt))
                   (list (strcat elt))
                   (otherwise elt))
          :into result
        :finally (return (uiop:reduce/strcat result))))

#+design-of-syntax
(examples
  (integer (sign digits dot))
  (sign (or "" #\+ #\-))
  (digits (or digit (digit digits)) :max 3)
  (digit (or . #.(coerce "1234567890" 'list)))
  (dot (or "" #\.)))

#+design-of-syntax
(examples
  (ratio (sign digits #\/ digits))
  (sign (or "" "+" "-"))
  (digit (or . #.(coerce "1234567890" 'list)))
  (digits (or digit (digit digits)) :max 3))
