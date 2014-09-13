#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|                               DEFINITIONS                               |
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

;;; Necessary to ensure that the double cons cells can be printed. Otherwise
;;; print enters an infinite loop continuously referencing the lefts and
;;; rights.
(setf *print-circle* t)

(defstruct dcons
  (value nil)
  (left nil)
  (right nil))

;;; dll := doubly linked list
(defstruct dll
  (head nil)
  (tail nil)
  (length 0))

(defun join-dcons (a b)
  (setf (dcons-left b) a)
  (setf (dcons-right a) b))

(defun make-dll-from-vals (&rest vals)
  (cond ((null (car vals)) (make-dll))
        (t (do* ((a (make-dcons :value (caar vals)) b)
                 (head a)
                 (len 1 (incf len))
                 (valb (cdar vals) (cdr valb))
                 ;; Overshoots by one here, i.e. makes one extra dcons.
                 (b (make-dcons :value (car valb))
                    (make-dcons :value (car valb)))
                 (tail a a))
             ((null valb)
              (make-dll :head head :tail tail :length len))
             (join-dcons a b)))))

;;; This idea for deep-copying lifted from here:
;;; http://rosettacode.org/wiki/Deepcopy#Common_Lisp
;;; Not sure if this is the best (or even correct) way for doing this.
(defun deep-copy (x) (read-from-string (princ-to-string x)))

;;; Fashioned after base lisp's mapcar.
;;; Heeds Siebel's warnings against leaky abstractions.
;;; http://www.gigamonkeys.com/book/macros-defining-your-own.html
(defmacro mapcar-dll (func adll)
  (let ((bdll (gensym)))
    `(let ((,bdll (deep-copy ,adll)))
       (do* ((this (dll-head ,bdll) (dcons-right this)))
         ((null this) ,bdll)
         (setf (dcons-value this) (funcall ,func (dcons-value this)))))))

(defmacro insert-dll (adll at-dcons new-dcons &key (after t))
  ;;  vn := value-name.
  ;;; Limitation: Whatever form goes into the value for after must evaluate to
  ;;; the same truth value every time. Don't know how to fix this using gensym.
  (let ((adll-vn (gensym))
        (at-dcons-vn (gensym))
        (new-dcons-vn (gensym))
        (at-left (gensym))
        (at-right (gensym)))
    `(let* ((,adll-vn ,adll)
            (,at-dcons-vn ,at-dcons)
            (,new-dcons-vn ,new-dcons)
            (,at-left ,(if after at-dcons-vn `(dcons-left ,at-dcons-vn)))
            (,at-right ,(if after `(dcons-right ,at-dcons-vn) at-dcons-vn)))
       (join-dcons ,at-left ,new-dcons-vn)
       (join-dcons ,new-dcons-vn ,at-right)
       (incf (dll-length ,adll-vn))
       ,adll-vn)))

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|                                  TESTS                                  |
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defvar a (make-dcons :value 1))
(defvar b (make-dcons :value 2))

(defvar two-dcons (join-dcons a b))

;;; Check the edge cases.
(defvar myvals0dll (make-dll-from-vals '()))
(defvar myvals1dll (make-dll-from-vals '(1)))

;;; Make something at least mildly interesting.
(defvar myvalsdll
  (make-dll-from-vals
    '((1 2 3) (4 5 6) (7 8 9))))

;(mapcar-dll #'1+ '(1 2 3)) ; Should throw an error.
(mapcar-dll #'1+ myvals1dll)
(mapcar-dll ; Do something complicated and also check for macro leaks.
  #'(lambda (x) (mapcar #'(lambda (bdll) (expt bdll 2)) x))
  myvalsdll)

(defvar new1 (make-dcons :value "This is the test value 1."))
(defvar new2 (make-dcons :value "This is the test value 2."))
(insert-dll myvalsdll (dll-head myvalsdll) new1 :after t)
(insert-dll myvalsdll (dll-tail myvalsdll) new2 :after nil)
