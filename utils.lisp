(in-package :xparser)
(defparameter *drawable-providers* '("drawable" "window" "pixmap"))
;;;; Utils
;;Called when generating clx, remove -1
(defun list->vector (list) ;fixme barfs on xinput which has two sets of events, probably change clx.
  "Slots an event or error into the proper slot in vector.
   Ignores negative and out of bound items."
  (let* ((vlist (remove -1 list :key #'cdadr))
	(vector (make-array (length vlist))))
    (dolist (e vlist vector)
      (let* ((cons (cadr e))
	     (index (cdr cons)))
	(if (/= -1 index)
	    (when (and (<= -1 index) (< index (length vector)))
			  (setf (svref vector index) e)))))))

(defun find-error (name &optional (ext *current*));fixme only works for a clx extension
  (find name (typecase (x-errors (x-code ext))
	       (cons  (x-errors (x-code ext)))
	       (extension (x-errors (x-code ext)))
	       (t nil)) :key (lambda (unit) (getf (cdar unit) :|name|)) :test #'string-equal))

(defun find-event (name &optional (ext *current*))
  (let ((events (typecase (x-events (x-code ext))
		  (cons (x-events (x-code ext)))
		  (extension (x-events (x-code ext)))
		  (t nil))))
    (find name events :key (lambda (unit) (getf (cdar unit) :|name|)) :test #'string-equal)))

(defun find-request (name &optional (ext *current*))
  (find name (typecase (x-code (x-code ext))
	       (cons 'ext)
	       (extension '(x-code ext))) :key #'caadr :test #'string-equal))

(defun compare (accessor-name &optional (n 0)  (ext *current*))
  "Prints out a slot recursively. Useful for debugging generators."
  (let ((access accessor-name))
    (when (/= 0 n)
      (setf access (lambda (x) (nth (1- n) (funcall accessor-name x)))))
    (when (typecase (x-code ext)
	    (extension (compare access 0 (x-code ext)))
	    (t )))
    (format t "~S~%~%" (funcall access ext))))

(defun get-size (type)
  (case (intern (string-upcase type))
    ((card8 int8 boolean) 1)
    ((card16 int16) 2)
    ((card64 int64) 8)
    (otherwise 4)));fixme make sure pads don't end up here

(defun n+  (&rest rest)
  (intern (string-upcase  (apply #'s+ rest)) 'xparser))
(defmacro f! (format-string &rest args)
 "Returns a string produced from given format-string and args." 
  `(with-output-to-string (string-stream) (format string-stream ,format-string ,@args)))
(defmacro with-keys (keys expr &body body)
  "KEYS = [ALIAS OR (KEY ALIAS)]*"
  (let (remapped-keys)
    (dolist (key keys)
      (if (consp key)
	  (push `((,(xlib::kintern (string-downcase (car key))) ,(intern (string-upcase (cadr key))))) remapped-keys)
	  (push `((,(xlib::kintern (string-downcase key)) (,@(intern  (string-upcase key))))) remapped-keys)))
    `(destructuring-bind (&key ,@(nreverse remapped-keys) &allow-other-keys) ,expr ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro generate (seed &rest cases)
    "Takes a seed,singular,e.g. xtype, and returns #'GENERATE-CLX-XTYPES which 
  case = (:key . value). current seed accessed through seed and extension through ext."
    (let ((fn-name (n+ "generate-clx-" seed "s"))
	  (accum (n+ seed "s")))
      `(defun ,fn-name (ext)
	 (let ((,accum '()))
	   (dolist (,(n+ seed) (,(n+ "x-" seed "s") ext))
	     (push (case (car ,(n+ seed)) ,@cases) ,accum))
	   (nreverse ,accum))))))
;; how many masks have non sequential bits besides xkb?
;; single bit masks in xkb FU!
;; fixme Reverse masks in xkb, more FU
;; xkb maybe fucking up, why reverse the order if bit value is the same? because they dumb, reverse it
(defun sequentialp (list &key (key #'cadadr))
  (if (and (< 1 (length list))
	   (= (parse-integer (cadadr (car list))) 0)) ;don't trust xkb shit
      (let ((num-list (mapcar #'parse-integer (mapcar key list))))
	(do* ((first (pop num-list) second)
	      (second (pop num-list)(pop num-list)))
	     ((and (endp num-list)
		   (= second (1+ first))) t)
	  (when (not (= second (1+ first)))
	    (return nil))))
      t))
(define-modify-macro reversef (&rest rest)
  reverse)


;;;names

;;no idea what licence this is or where it comes from besides pastebin
(defun lispify (camel-string)
  "Insert - between lowercase and uppercase chars.
Ignore _ + * and several consecutive uppercase.
simplified-camel-case by Leslie P. Polzer "
  (declare (string camel-string))
  (let ((*print-pretty* nil))
    (with-output-to-string (result)
      (loop for c across camel-string
	 with last-was-lowercase
	 when (and last-was-lowercase
		   (upper-case-p c))
	 do (princ "-" result)
	 if (lower-case-p c)
	 do (setf last-was-lowercase t)
	 else
	 do (setf last-was-lowercase nil)
	 do (princ (case c
		     ((#\: #\_) "-")
		     (t c)) result)))))
(defun s+ (&rest parts)
  (apply #'concatenate 'string (mapcar #'string parts)))
(defun extensify-name (name ext-name)
  "Returns name prepended by short extension name + '- based on *current* extension."
  (s+ ext-name '- (string-downcase name)))
(defun xname (string)
  "Takes a string and returns a lispified symbol."
  (intern (format nil "~a" (lispify string))))
(defun getify (name)
  (s+ name "-get"))
(defun putify (name)
  (s+ name "-put"))
