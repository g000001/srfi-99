;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-99
  (:use)
  (:export
   :rtd?
   :record? :record-rtd
   :rtd-name :rtd-parent
   :rtd-field-names :rtd-all-field-names :rtd-field-mutable?
   :make-rtd
   :rtd-constructor
   :rtd-predicate
   :rtd-accessor
   :rtd-mutator
   :define-record-type
   ))

(defpackage :srfi-99.internal
  (:use :srfi-99 :cl :fiveam :mbe) )
