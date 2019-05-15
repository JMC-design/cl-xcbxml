(in-package :xparser)
;; lxml output gives us a list of self contained units to be parsed.
;; Why overcomplicate?

(defun parse-attribute-name (string)
  "Default parser for the attribute name"
  (declare (special *namespaces*))
  (resolve-identifier (lispify string) *namespaces* t))

(defun parse-attribute-value (name string)
  "Default parser for the attribute value"
  (declare (ignore name)
           (special *ignore-namespace*))
  (if *ignore-namespaces*
      (copy-seq (lispify string))
      (lispify string)))
(setf s-xml:*attribute-name-parser* #'parse-attribute-name
      s-xml:*attribute-value-parser* #'parse-attribute-value)

(defun split-code (code)
  (let ((ext (make-extension :code code)))
    (dolist (unit code)
      (case (type-of (car unit))
	(keyword
	 (case (car unit)
	   (:|xcb|     (push unit (x-name ext)))
	   (:|import|  (push unit (x-name ext)))
	   (t (error "Unknown argument ~a" (car unit)))))
	(cons
	 (if (consp (car unit))
	     (case (caar unit)
	       (:|enum|        (push unit (x-enums ext))) 
	       (:|typedef|     (push unit (x-types ext))) 
	       (:|xidtype|     (push unit (x-types ext)))
	       (:|xidunion|    (push unit (x-types ext)))
	       (:|union|       (push unit (x-structs ext)))
	       (:|error|       (push unit (x-errors ext)))
	       (:|errorcopy|   (push unit (x-errors ext)))
	       (:|struct|      (push unit (x-structs ext)))
	       (:|request|     (push unit (x-requests ext)))
	       (:|event|       (push unit  (x-events ext)))
	       (:|eventcopy|   (push unit  (x-events ext)))
	       (:|eventstruct| (push unit  (x-events ext)))		 
	       (t (error "Unknown argument ~a" (caar unit))))
	     (push unit (x-unknown ext)))))) ;so we know what we don't know.
    (reversef (x-requests ext))
    ext))

(defun parse-extension (code)
  (let ((ext (make-extension :code (split-code code) :constants '()))
	(temp code))
    (setf *current* ext);fixme this is most likely why FIND-thing barfs sometimes
    (dolist (unit code)
      (case (type-of (car unit))
	       (keyword
		(case (car unit)
		  (:|xcb|     (multiple-value-bind (name major minor) (parse-header (cdr unit))
				(progn (setf (x-name ext) name)
				       (push major (x-constants ext))
				       (push minor (x-constants ext)))))
		  (:|import|  (parse-import unit))
		  (t (error "Unknown argument ~a" (car unit)))))
	       (cons
		(case (caar unit)
		  (:|enum|      (push (parse-enum unit) (x-enums ext))) 
		  (:|typedef|   (push (parse-typedef unit) (x-types ext))) 
		  (:|xidtype|   (push (parse-xidtype unit) (x-types ext)))
		  (:|xidunion|  (push (parse-xidunion unit) (x-types ext)))
		  (:|union|     (push (parse-union unit) (x-structs ext)))
		  (:|error|     (push (parse-xerror unit) (x-errors ext)))
		  (:|errorcopy| (push (parse-errorcopy unit ext) (x-errors ext)))
		  (:|struct|    (push (parse-struct unit) (x-structs ext)))
		  (:|request|   (push (parse-request unit) (x-requests ext)))
		  (:|event|     (push (parse-event unit ext)  (x-events ext)))
		  (:|eventcopy| (push (parse-eventcopy unit ext)  (x-events ext)))
		  (:|eventstruct| (push (parse-eventstruct unit)  (x-events ext)))		  
		  (t (error "Unknown argument ~a" (caar unit)))))))
    (reversef (x-requests ext))
    ext))

(defun load-extension (filename)
  (setf *current* (parse-extension (parse-xml-file filename))))

(defun clx-load (filename)
  (setf *current* (ext->clx (parse-extension (parse-xml-file filename)))))


