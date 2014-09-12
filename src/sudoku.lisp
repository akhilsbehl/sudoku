;;; Directly lifted from Knuth's code.
(defparameter +max-level+ 150)
(defparameter +max-degree+ 1e3)
(defparameter +max-cols 1e4)
(defparameter +max-nodes 1e6)

(defstruct node
  up
  down
  left
  right
  column)

(defstruct (column :include node)
  head
  name
  size
  next
  prev)

(defparameter +column-array+
  (make-array (+ 2 +max-cols+)
              :element-type 'column
              :initial-element (make-column)))

(defparameter +node-array+
  (make-array +max-nodes+
              :element-type 'node
              :initial-element (make-node)))

(defparameter +root+ (aref +column-array+ 0))

(defun dlx-search (k)
  (cond ((eq (dlx-right root) root) (dlx-print))
        (t (let ((c (dlx-choose-column)))
             (dlx-cover c)
             (do* ((r (dlx-down c) (dlx-down r))
                   (ok r r))
               ((eq r c))
               (do ((j (dlx-right r) (dlx-right j)))
                 ((eq j r))
                 (dlx-cover j))
               (dlx-search (1+ k))
               (setf r ok)
               (setf c (dlx-header c))
               (do (j (dlx-left r) (dlx-left j))
                 ((eq j r))
                 (dlx-uncover j)))
             (dlx-uncover c)))))
