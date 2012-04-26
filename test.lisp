;;;; srfi-99.lisp

(cl:in-package :srfi-99.internal)
;; (in-readtable :srfi-99)

(def-suite srfi-99)

(in-suite srfi-99)

;;; "srfi-99" goes here. Hacks and glory await!

(define-record-type foo
                    t
                    t
  a)

#|(define-record-type (bar foo)
                    (mkbar a b c z)
                    t
  (z bar/z set/bar/z)
  q)|#


(define-record-type (bar foo)
                    t
                    t
  b)

(define-record-type (baz bar)
                    t
                    t
  (c))


(rtd-field-names (find-class 'bar))
;=>  #(B)

(rtd-all-field-names (find-class 'bar))
;=>  #(A B)

(funcall (rtd-predicate 'foo)
         (make-foo 1))
;=>  T
((rtd-constructor bar (rtd-all-field-names bar)) 1 2 3 4)

(funcall (rtd-constructor (find-class 'bar)
                          ;; (rtd-all-field-names (find-class 'bar))
                          #(a)
                          )
         2)
;=>  #S(BAR :.RTD-INFO. NIL :A 2 :B NIL)

(let ((obj (make-instance (find-class 'bar))))
  (map nil
       (lambda (name val) (setf (slot-value obj name)
                                val))
       #(a)
       '(1))
  obj)


(typep (make-foo 8) 'structure-class)

(describe 'foo)


(sb-mop::class-slots  (find-class 'foo) )
;=>  (#<SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION .RTD-INFO.>
;     #<SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION A>)

(mapcar (lambda (s) (slot-value s 'sb-int::info))
        (c2mop:class-slots (find-class 'baz)))

;=>  (#<SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION .RTD-INFO.>
;     #<SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION A>)

(c2mop:class-direct-superclasses (find-class 'bar))
;=>  (#<STRUCTURE-CLASS FOO>)


(c2mop:accessor-method-slot-definition #'baz-c)
(mapcar #'c2mop:accessor-method-slot-definition
        (c2mop:class-slots (find-class 'baz)))
()


(funcall (rtd-accessor (find-class 'bar) 'a)
         (make-bar 9))

( )
( )

(mapcar (lambda (x)
          (sb-kernel::dsd-read-only x))
        (sb-pcl::structure-type-slot-description-list (find-class 'bar)))


(mapcar (lambda (x)
              (cons x (sb-kernel::dsd-read-only x)))
            slotds)



(describe (sb-kernel::find-defstruct-description 'bar))

(mapcar (lambda (s)
           )
        (c2mop:class-slots (find-class 'baz)))



(sbmop::type  (find-class 'bar))

(define-record-type (bar foo)
                    t
                    t
  b)



(make-bar 8)


;=>  #S(BAR :.RTD-INFO. NIL :A NIL :B 8)

(make-rtd-info :constructor 8)

(let ((r (make-baz 3)))
  (funcall (rtd-mutator (find-class 'baz) 'z) r 'x)
  (describe r))

(make-rtd 'zot #((q)) (find-class 'baz))
(PROGN
  (DEFSTRUCT (ZOT (:INCLUDE BAZ)) (Q))
 (PROGN
  (SETF (SYMBOL-FUNCTION 'ZOT-Q) (LAMBDA (OBJ) (SLOT-VALUE OBJ 'Q)))
  (SETF (SYMBOL-FUNCTION 'ZOT-Q-SET!)
          (LAMBDA (OBJ VALUE) (SETF (SLOT-VALUE OBJ 'Q) VALUE))))
 'ZOT)

(make-instance 'structure-class
               ;; :direct-default-initargs nil
               :direct-slots
               (list (list
                      (slot-value (find-class 'baz) 'sb-pcl::direct-slots)))
               ;; :direct-superclasses nil
               :name 'q
               )

(sb-kernel::make-defstruct-slot-description
                            :name 'dum16
                            :index 10
                            :accessor-name 'dum18
                            :default 'dum19
                            :type 'dum20
                            :safe-p t
                            :raw-type t
                            :read-only nil)

(let ((q (make-instance 'structure-class
                        :direct-slots nil
                        :name 'q)))
  (make-instance q))

(let ((q (make-instance 'structure-class
                        ;; :direct-slots nil
                        :name 'foo)))
  (SB-KERNEL:%MAKE-STRUCTURE-INSTANCE q nil))

(describe (sb-kernel::make-defstruct-description 'z))

(sb-kernel::make-defstruct-description 'z)

(deftype z ()
  'structure-class)

(SB-KERNEL:%MAKE-STRUCTURE-INSTANCE
 (sb-kernel::make-defstruct-description 'z)
 nil)
