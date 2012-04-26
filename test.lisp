;;;; srfi-99.lisp

(cl:in-package :srfi-99.internal)

(def-suite srfi-99)

(in-suite srfi-99)

(define-record-type foo
                    t
                    t
  a)

(define-record-type (bar foo)
                    t
                    t
  b)

(define-record-type (baz bar)
                    t
                    t
  (c))

(test srfi-99
  (is (equalp (rtd-field-names (find-class 'foo))
              #(a)))
  (is (equalp (rtd-field-names (find-class 'bar))
              #(b)))
  (is (equalp (rtd-field-names (find-class 'baz))
              #(c)))
  (is (equalp (rtd-all-field-names (find-class 'foo))
              #(A)))
  (is (equalp (rtd-all-field-names (find-class 'bar))
              #(A B)))
  (is (equalp (rtd-all-field-names (find-class 'baz))
              #(A B C)))
  (is-true (funcall (rtd-predicate (find-class 'foo))
                    (make-foo 1)))
  (is (equalp (funcall (rtd-constructor (find-class 'bar)
                          (rtd-all-field-names (find-class 'bar))
                          )
                       nil 2)
              (make-bar 2))))
;;; eof
