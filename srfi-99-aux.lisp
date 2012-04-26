(cl:in-package :srfi-99.internal)

(use-package :mbe)

(defun fintern (fmt &rest args)
  (intern (apply #'format nil fmt args)))

(defstruct (type-spec
            (:constructor %make-type-spec (type-name parent)))
  type-name
  parent)

(defun make-type-spec (type-name)
  (cond ((consp type-name)
         (%make-type-spec (first type-name)
                          `((:include ,(second type-name)))))
        ;; (T (%make-type-spec type-name '((:include rtd))))
        (T (%make-type-spec type-name nil))))

(defstruct (constructor-spec
            (:constructor %make-constructor-spec (constructor-name field-name)))
  constructor-name
  field-name)

(defun make-constructor-spec (spec type-name)
  (case spec
    ((t) (%make-constructor-spec (fintern "MAKE-~A" type-name) nil))
    ((nil) nil)
    (otherwise (etypecase spec
                 (cons (%make-constructor-spec (car spec)
                                               (cdr spec)))
                 (symbol (%make-constructor-spec spec nil))))))

(defstruct (predicate-spec
            (:constructor %make-predicate-spec (predicate-name)))
  predicate-name)

(defun make-predicate-spec (predicate-name type-name)
  (case predicate-name
    ((nil) (%make-predicate-spec nil))
    ((t) (%make-predicate-spec `((:predicate ,(fintern "~A?" type-name)))))
    (otherwise (%make-predicate-spec `((:predicate ,predicate-name))))))

(defstruct (field-spec
            (:constructor %make-field-spec (name accessor mutator read-only
                                                 mode
                                                 default-accessor)))
  name accessor mutator read-only mode default-accessor)

(defun make-field-spec (spec type-name)
  (etypecase spec
    (symbol (%make-field-spec spec
                              (fintern "~A-~A" type-name spec)
                              nil
                              t
                              :default-none
                              (fintern "~A-~A" type-name spec) ))
    (cons (cond ((null (cdr spec))
                 ;; default/default
                 (%make-field-spec (first spec)
                                   (fintern "~A-~A" type-name (first spec))
                                   (fintern "~A-~A-SET!" type-name (first spec))
                                   nil
                                   :default-default
                                   (fintern "~A-~A" type-name (first spec)) ))
                ((null (cddr spec))
                 ;; specified/default
                 (%make-field-spec (first spec)
                                   (second spec)
                                   (fintern "~A-~A-SET!" type-name (first spec))
                                   nil
                                   :specified-default
                                   (fintern "~A-~A" type-name (first spec)) ))
                (T
                 ;; specified/specified
                 (%make-field-spec (first spec)
                                   (second spec)
                                   (third spec)
                                   nil
                                   :specified-specified
                                   (fintern "~A-~A" type-name (first spec)) ))))))

(defun field-spec-expr (spec)
  `(,(field-spec-name spec)
     ,@(and (field-spec-read-only spec) (list nil :read-only T))))

(defun make-defstruct-form (type-spec
                            constructor-spec
                            predicate-spec
                            field-spec )
  (let* ((type-spec (make-type-spec type-spec))
         (type-name (type-spec-type-name type-spec))
         (constructor-spec (make-constructor-spec constructor-spec type-name))
         (predicate-spec (make-predicate-spec predicate-spec type-name))
         (field-spec (mapcar (lambda (s) (make-field-spec s type-name))
                             field-spec ))
         #|(rtd-info (make-rtd-info
                    :parent (type-spec-parent type-spec)
                    :mutator (mapcar #'field-spec-mutator field-spec)
                    :accessor (mapcar #'field-spec-accessor field-spec)
                    :name (type-spec-type-name type-spec)
                    :field-names (mapcar #'field-spec-name field-spec)
                    :constructor (constructor-spec-constructor-name
                                  constructor-spec)
                    :predicate (predicate-spec-predicate-name
                                predicate-spec)
                    ;;:all-field-names
                    #|(list
                     (if (type-spec-parent type-spec)
                         (all-field-names
                          (type-spec-parent type-spec) ))
                     (mapcar #'field-spec-name field-spec) )|#
                    ))|#)
    `(progn
       (defstruct (,type-name
                   ,@(type-spec-parent type-spec)
                   ,@(let ((ctor constructor-spec))
                       (and ctor
                            `((:constructor
                               ,(constructor-spec-constructor-name ctor)
                               ,(if (constructor-spec-field-name ctor)
                                    (constructor-spec-field-name ctor)
                                    `(,@(mapcar #'field-spec-name field-spec)
                                        ;; &aux (.rtd-info. ,rtd-info)
                                        ))))))
                   ,@(predicate-spec-predicate-name predicate-spec) )
         ,@(mapcar #'field-spec-expr field-spec))
       ,@(mapcar #'accessor/modifier-expr field-spec)
       ',type-name)))

(defun accessor/modifier-expr (spec)
  (case (field-spec-mode spec)
    (:default-none nil)
    (:default-default
        `(progn
           (setf (symbol-function ',(field-spec-accessor spec))
                 (lambda (obj)
                   (slot-value obj ',(field-spec-name spec)) ))
           #|(and (fboundp '(setf ,(field-spec-accessor spec)))
                (fmakunbound '(setf ,(field-spec-accessor spec))))|#
           (setf (symbol-function ',(field-spec-mutator spec))
              (lambda (obj value)
                (setf (slot-value obj ',(field-spec-name spec))
                      value )))))
    (:specified-default
     `(progn
        (setf (symbol-function ',(field-spec-accessor spec))
              (lambda (obj)
                (slot-value obj ',(field-spec-name spec)) ))
        #|(and (fboundp ',(field-spec-default-accessor spec))
             (fmakunbound ',(field-spec-default-accessor spec)))|#
        (setf (symbol-function ',(field-spec-mutator spec))
              (lambda (obj value)
                (setf (slot-value obj ',(field-spec-name spec))
                      value )))))
    (:specified-specified
     `(progn
        (setf (symbol-function ',(field-spec-accessor spec))
              (lambda (obj)
                (slot-value obj ',(field-spec-name spec)) ))
        #|(and (fboundp ',(field-spec-default-accessor spec))
             (fmakunbound ',(field-spec-default-accessor spec)))|#
        (setf (symbol-function ',(field-spec-mutator spec))
              (lambda (obj value)
                (setf (slot-value obj ',(field-spec-name spec))
                      value )))
        #|(and (fboundp '(setf ,(field-spec-default-accessor spec)))
             (fmakunbound '(setf ,(field-spec-default-accessor spec))))|#))))

;;; eof
