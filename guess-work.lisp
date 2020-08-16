;;;; guess-work.lisp

(in-package #:guess-work)

(defclass situation ()
  ((label :reader label
          :initarg :label
          :initform (error "must provide a label"))))

(defun slot-names (instance &optional class)
  (sort 
   (remove 'label
           (mapcar #'closer-mop:slot-definition-name
                   (closer-mop:class-slots
                    (if class instance (class-of instance)))))
   #'string<
   :key #'symbol-name))

(defvar *considering-slots* nil)

(defun slot-conj->number (&rest slots)
  (loop
     :for s :in *considering-slots*
     :for pow :from (1- (length *considering-slots*)) :downto 0
     :summing (* (if (member s slots) 1 0)
                 (expt 2 pow))))

(defun situation->list (situaton)
  (mapcar (lambda (s) (slot-value situaton s))
          (or *considering-slots* (slot-names situaton))))

(defun collect-slots (expr)
  (remove-if-not (lambda (e) (and (symbolp e) (not (keywordp e)))) expr))

(defun collect-subexpressions (op expr)
  (remove-if-not (lambda (e) (and (consp e) (eql op (car e)))) expr))


(defun merge-like (thing more-things)
  (append thing (apply #'append (mapcar #'cdr more-things))))

(defun list-product (lists)
  (if (null lists) (list nil)
      (loop
         :for head :in (car lists)
         :nconc (loop
                   :for tail :in (list-product (cdr lists))
                   :collect (cons head tail)))))

(defun conj-of-disj (disjs)
  (if (and (consp disjs) (cdr disjs)) ;; length >= 2 
      (cons :or
            (mapcar (lambda (ls) (cons :and ls))
                    (list-product (mapcar #'cdr disjs))))
      (car disjs)))


(defun list? (x)
  (if (listp x) x (list x)))

(defun distribute-over-disj (conj disj)
  (when disj 
    (cons :or
          (mapcar (lambda (term) (canonicalize (cons :and (append (list? term) (cdr conj)))))
                  (cdr disj)))))

(defun canonicalize (expr)
  (cond ((atom expr) expr)
        ((and (consp expr) (eql :and (car expr)))
         (let* ((subcan (mapcar #'canonicalize (cdr expr)))
                (disj  (conj-of-disj (collect-subexpressions :or subcan)))
                (conj   (merge-like
                         (cons :and (collect-slots subcan))
                         (collect-subexpressions :and subcan)))
                (distributed (distribute-over-disj conj disj)))
           (if distributed distributed conj)))
        ((and (consp expr) (eql :or (car expr)))
         (let* ((subcan (mapcar #'canonicalize (cdr expr)))
                (disj (merge-like
                       (cons :or (collect-slots subcan))
                       (collect-subexpressions :or subcan))))
           (nconc disj (collect-subexpressions :and subcan))))
        (t (error "what?"))))


;; TODO handle case of single symbol, single :and form, and single :or
;; form filled soley with slot names
(defun canonical-to-rule (canonical)
  (let* ((canonical (if (eql :and (car canonical))  (list :or canonical)
                        (cons :or (mapcar (lambda (x) (if (atom x) (list :and x) x))
                                          (cdr canonical)))))
         (conj-preds (loop :for conj :in (cdr canonical)
                        :collect (apply #'slot-conj->number (cdr conj)))))
    (lambda (n)
      (loop
         :for pred :in conj-preds
         :when (conj-check pred n) :do  (return t)
         :finally (return nil)))))

(defun make-rule (expr)
  (assert *considering-slots* nil
          "MAKE-RULE must be called within the body of WITH-SITUATION-CLASS")
  (canonical-to-rule (canonicalize expr)))

(defun bvec->number (v)
  (loop
     :for b :across v
     :for pow :from (1- (length v)) :downto 0
     :summing (* b (expt 2 pow))))

(defun conj-check (pred sample)
  (zerop (logand pred (logxor pred sample))))

(defun pop-random (ls)
  (labels ((rec (front idx back)
             (if (zerop idx)
                 (values (car back) (nconc (reverse front) (cdr back)))
                 (rec (cons (car back) front)
                      (1- idx)
                      (cdr back)))))
    (rec nil (random (length ls)) ls)))

(defun n-distinct-between (n &key (low 0) (high 100))
  (let ((nums (loop :for x :from low :below high :collect x))
        (collected nil))
    (dotimes (ignore n collected)
      (multiple-value-bind (val new-nums) (pop-random nums)
        (push val collected)
        (setf nums new-nums)))))

(defun generate (situation &key (sample-size 100))
  (let* ((width (length *considering-slots*))
         (table (make-array (list sample-size width) :element-type 'bit)))
    (loop
       :for prob :in (situation->list situation)
       :for key :from 0 
       :do
         (dolist (idx (n-distinct-between (floor (* prob sample-size)) :high sample-size))
           (setf (aref table idx key) 1)))
    (let ((row (make-array width
                           :element-type 'bit
                           :displaced-to table
                           :displaced-index-offset 0
                           :adjustable t)))
      (loop :for i :below sample-size
         :collect (bvec->number row)
         :when (< i (1- sample-size))
         :do  (adjust-array row width
                            :element-type 'bit
                            :displaced-to table
                            :displaced-index-offset (* (1+ i) width))))))

(defun run-simulation (situation rule &key (size 100))
  (assert *considering-slots* nil
          "RUN-SIMULATION must be called within body of WITH-SITUATION-CLASS")
  (let ((rule (if (functionp rule) rule (make-rule rule)))
        (samples (generate situation :sample-size size)))
    (/ (loop :for s :in samples
          :when (funcall rule s) :count s)
       (* 1.0 size))))

(defun run-and-print-simulation-on-data (rule data &key (size 100) (sort-by #'>))
  (assert *considering-slots* nil
          "RUN-AND-PRINT-SIMULATION-ON-DATA must be called within body of WITH-SITUATION-CLASS")
  (let* ((rule (if (functionp rule) rule (make-rule rule)))
         (table 
          (sort 
           (loop
              :for situation :in data
              :collect (cons (slot-value situation 'label)
                             (run-simulation situation rule :size size)))
           sort-by :key #'cdr)))
    (mapc (lambda (pair) (format t "~40a: ~a~%" (car pair) (cdr pair))) table)
    (values)))



