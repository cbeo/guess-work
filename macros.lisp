(in-package :guess-work)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (symbol)
    (read-from-string (concatenate 'string ":" (symbol-name symbol)))))

(defmacro with-situation-class ((class) &rest body)
  `(let ((*considering-slots* (slot-names (find-class ',class) t)))
     ,@body))

(defmacro defsituation (name &rest slots)
  (let ((slots (if (stringp  (car slots)) (cdr slots) slots))
        (docstring (when (stringp (car slots)) (car slots))))
    `(progn
       (defclass ,name (situation)
         ,(loop :for slot :in slots
             :collect `(,slot :initarg ,(make-keyword slot)
                              :initform 0))
         ,@(when docstring (list  (list :documentation docstring))))
       
       (defun ,name (label &key ,@(mapcar (lambda (s)  (list s 0)) slots))
         (apply 'make-instance ',name :label label
                ,(cons 'list (loop :for slot :in slots
                                :append (list (make-keyword slot) slot))))))))

