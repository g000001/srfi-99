(cl:in-package :srfi-99.internal)

(declaim
 (ftype (function (symbol vector &optional (or structure-class nil)) t) make-rtd)
 (ftype (function (t) boolean) rtd?)
 (ftype (function (structure-class &optional vector) function) rtd-constructor)
 (ftype (function (structure-class) function) rtd-predicate)
 (ftype (function (structure-class symbol) function) rtd-accessor)
 (ftype (function (structure-class symbol) function) rtd-mutator)
 (ftype (function (t) boolean) record?)
 (ftype (function (structure-object) t) record-rtd)
 (ftype (function (structure-class) t) rtd-name)
 (ftype (function (structure-class) structure-class) rtd-parent)
 (ftype (function (structure-class) simple-vector) rtd-field-names)
 (ftype (function (t) t) struct-slot-description-name)
 (ftype (function (t) boolean) struct-slot-description-read-only-p)
 (ftype (function (t) boolean) struct-slot-description-read-only-p)
 (ftype (function (structure-class symbol) boolean) rtd-field-mutable?)
 (ftype (function (structure-class) list) defstruct-ancestors)
 (ftype (function (t) t) structure-type-slot-description-list)
 (ftype (function (structure-class) list) defstruct-all-slotds)
 (ftype (function (vector) list) ensure-fields)
 )

(defmacro define-record-type (type constructor predicate &body fields)
  (make-defstruct-form type constructor predicate fields))

(defun ensure-fields (fields)
  (map 'list
       (lambda (fld)
         (etypecase fld
           (cons (if (cdr fld)
                     (case (car fld)
                       (:mutable (cdr fld))
                       (:immutable (cadr fld)))
                     fld))
           (symbol fld)))
       fields))

(defun make-defstruct-description (name)
  #+sbcl (sb-kernel::make-defstruct-description name)
  #-sbcl :not-implemented)

;;; FIXME FIXME FIXME FIXME FIXME
(defun make-rtd (name fieldspecs &optional parent)
  (let ((dd (make-defstruct-description name)))
    #+sbcl (setf (slot-value dd 'sb-kernel::include)
                 (list parent)
                 (slot-value dd 'sb-kernel::slots)
                 (ensure-fields fieldspecs))
    dd))

(defun rtd? (obj)
  (typep obj 'cl:structure-class))

;;; (rtd-constructor rtd &optional fieldspecs)
#|(defun rtd-constructor (rtd &optional fieldspecs)
  (let ((arglen (length fieldspecs)))
    (lambda (&rest args)
      (assert (= arglen (length args)))
      (let ((obj (make-instance rtd)))
        (map nil
             (lambda (name val)
               (setf (slot-value obj name) val))
             fieldspecs
             args)
        obj))))|#

(defun rtd-constructor (rtd &optional fieldspecs)
  (let ((args (coerce fieldspecs 'list)))
    (Coerce `(lambda (,@args)
               (let ((obj (make-instance ',(class-name rtd))))
                 (map nil
                      (lambda (name val)
                        (setf (slot-value obj name) val) )
                      ',fieldspecs
                      (list ,@args) )
                 obj ))
            'cl:Function )))

;;; (rtd-predicate rtd)
(defun rtd-predicate (rtd)
  (lambda (obj) (typep obj rtd)))

;;; (rtd-accessor rtd field)
(defun rtd-accessor (rtd field)
  (assert (find field (rtd-all-field-names rtd)))
  (lambda (obj)
    (assert (typep obj rtd))
    (slot-value obj field)))

;;; (rtd-mutator rtd field)
(defun rtd-mutator (rtd field)
  (if (rtd-field-mutable? rtd field)
      (lambda (obj value)
        (assert (typep obj rtd))
        (setf (slot-value obj field) value))
      (error "slot ~A of record ~A is immutable" field rtd)))

;;; (record? obj)
(defun record? (obj)
  (typep obj 'cl:structure-object))

;;; (record-rtd record)
(defun record-rtd (record)
  ;; (assert (record? record))
  (class-of record))

;;; (rtd-name rtd)
(defun rtd-name (rtd)
  (class-name rtd))

;;; (rtd-parent rtd)
(defun rtd-parent (rtd)
  (first (c2mop:class-direct-superclasses rtd)))

;;; (rtd-field-names rtd)
(defun rtd-field-names (rtd)
  (map 'simple-vector
       #'c2mop:slot-definition-name
       (c2mop:class-direct-slots rtd)))

;;; (rtd-all-field-names rtd)
(defun rtd-all-field-names (rtd)
  (map 'vector
       #'c2mop:slot-definition-name
       (c2mop:class-slots rtd)))

(defun struct-slot-description-name (sd)
  #+sbcl (sb-kernel::dsd-name sd)
  #-sbcl :not-implemented)

(defun struct-slot-description-read-only-p (sd)
  #+sbcl (sb-kernel::dsd-read-only sd)
  #-sbcl :not-implemented)

;;; (rtd-field-mutable? rtd field)
(defun rtd-field-mutable? (rtd field)
  (let* ((slotds (defstruct-all-slotds rtd))
         (slot-found-p nil)
         (read-only-p
          (find-if (lambda (s)
                     (and (eq (struct-slot-description-name s)
                              field)
                          (setq slot-found-p T)
                          (not (struct-slot-description-read-only-p s))))
                   slotds )))
    (if slot-found-p
        (and read-only-p T)
        (error "rtd-mutable?: ~A does not have a slot ~A"
               rtd field))))

(defun defstruct-ancestors (rtd)
  (let* ((top (find-class 'structure-object))
         (cpl (c2mop:class-precedence-list rtd)))
    (subseq cpl 0 (position top cpl))))

(defun structure-type-slot-description-list (type)
  #+sbcl (sb-pcl::structure-type-slot-description-list type)
  #-sbcl :not-implemented)

(defun defstruct-all-slotds (rtd)
  (let* ((cs `(,rtd ,@(defstruct-ancestors rtd)))
         (cnames (mapcar #'class-name cs)))
    (mapcan (lambda (x)
              (copy-list (structure-type-slot-description-list x)))
            cnames)))

;;; eof
