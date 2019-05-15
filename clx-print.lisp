(in-package :xparser)

(defun generic-print (item &optional (ext *current*) (stream *standard-output*) (eval nil))
  (let* ((accessor (n+ 'x- item))
	 (item-list (funcall accessor ext)))
    (when item-list
      (format stream ";;; ~a~%" item)
      (when eval (format stream "(eval-when (:compile :load-toplevel :execute)~%"))
      (format stream "~{~a~%~}" item-list)
      (when eval (format stream ")~%"))
      (terpri stream))))

(defun print-licence (ext stream)
  (declare (ignore ext))
  (format stream ";; Licence: MIT~%")
  (format stream "(in-package :xlib)~%"))  

(defun print-exports (ext stream)
  (format stream "(export '(~{~a~^~%~10T~}))~%~%" (x-exports ext)))

(defun print-extension-definition (ext stream)
  (let ((name (getf (x-name ext) :xname))
	(events  (map 'list (lambda (e) (when (listp e) (caadr e))) (x-event-vector ext)));xproto ugliness
	(errors (map 'list (lambda (e) (when (listp e) (caadr e))) (x-error-vector ext))))
    (format stream "(define-extension ~S" name)
    (when events (format stream "~%  :events (~{:~a~^~%~11T~})" events))
    (when errors (format stream "~%  :errors (~{~a~^~%~11T~})" errors))
    (format stream ")~%~%")))

(defun print-clx-extension (ext &optional (stream *standard-output*))
  "Takes an extension and prints out a clx appropriate file."
  (print-licence ext stream)
  (print-exports ext stream)
  (print-extension-definition ext stream)
  (generic-print 'constants ext stream t)
  (generic-print 'enums ext stream t)
  (mapcan (lambda (type) (generic-print type ext stream)) '(types structs events errors requests)))
