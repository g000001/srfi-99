;;;; srfi-99.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-99
  :serial t
  :depends-on (:fiveam :closer-mop :mbe)
  :components ((:file "package")
               (:file "srfi-99-aux")
               (:file "srfi-99")
               ;; (:file "test")
               ))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-99))))
  (load-system :srfi-99)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-99.internal :srfi-99))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
