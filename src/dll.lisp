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
  (cond
    ((and (null a) (null b)) nil)
    ((null a) (setf (dcons-left b) nil))
    ((null b) (setf (dcons-right a) nil))
    (t (progn
         (setf (dcons-left b) a)
         (setf (dcons-right a) b)))))

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

(defun insert-dll (new-dcons in-dll &optional (at-dcons nil) (after t))
  (cond ((null (dll-head in-dll)) ; empty list
         (progn
           (setf (dll-head in-dll) new-dcons)
           (setf (dll-tail in-dll) new-dcons)
           (incf (dll-length in-dll))))
        (t (cond (after ; insert after.
                   (let ((reset (null (dcons-right at-dcons))))
                     (join-dcons new-dcons (dcons-right at-dcons))
                     (join-dcons at-dcons new-dcons)
                     (incf (dll-length in-dll))
                     (if reset (setf (dll-tail in-dll) new-dcons))))
                 (t ; insert before.
                   (let ((reset (null (dcons-left at-dcons))))
                     (join-dcons (dcons-left at-dcons) new-dcons)
                     (join-dcons new-dcons at-dcons)
                     (incf (dll-length in-dll))
                     (if reset (setf (dll-head in-dll) new-dcons))))))))

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

(defvar empty (make-dll))
(insert-dll (make-dcons :value "It used to be lonely here.") empty)

(insert-dll (make-dcons :value "AfterHead.") myvalsdll (dll-head myvalsdll) t)
(insert-dll (make-dcons :value "BeforeTail.") myvalsdll (dll-tail myvalsdll) nil)
(insert-dll (make-dcons :value "BeforeHead") myvalsdll (dll-head myvalsdll) nil)
(insert-dll (make-dcons :value "AfterTail") myvalsdll (dll-tail myvalsdll) t)
