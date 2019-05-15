(in-package :xparser)

(defmacro unit-name (unit)
  `(cddar ,unit))

;; Only in xinput ;makes find-event barf
(defun parse-eventstruct (unit)
  unit)
;; unions are structs that take up the same amount of space, useful in C, not currently useful
;; in clx, possibly for pipelines so don't have to deal with parsing pads during extraction.
(defun parse-union (unit);randr uses for mapping events to structs
  unit)
;; Everything currently gets shoved into the global clx namespace so not necessary right now.
(defun parse-import (unit)
  unit)
(defun parse-enum (unit) ;FIXME make sure event structs get shoved into events
					;(when (consp (car (cadadr unit))))
  (let ((name (car (unit-name unit)))
	(type (car (cadadr unit)))
	(fields (sort (remove :|doc| (cdr unit) :key #'car) #'< :key (lambda (x) (parse-integer (cadadr x)))))) ;sort because stupid xkb but barfs on docs
    (case type
      (:|value| `(:set ,name ,@(let ((result '()))
				 (dolist (field fields (nreverse result))
				   (when (listp (car field)) ;skip docs
				     (push `(,(third (car field))  ,(parse-integer (cadadr field))) result))))))
      (:|bit| (if (sequentialp fields)`(:mask ,name ,@(map 'vector (lambda (field) (when (listp (car field)) (caddar field))) fields))
		  `(:fixme ,name ,type ,fields))))))

(defun parse-typedef (unit)
  (with-keys (oldname newname) (cdar unit) `(:type (,newname . ,oldname))))
(defun parse-xidtype (unit)`(:type (,@(cddar unit) . "RESOURCE-ID")))
(defun parse-xidunion (unit) unit);glx -should these be structs in clx?
(defun parse-header (unit)
  (with-keys (header extension-xname extension-name major-version minor-version) unit
    (values `(:short ,header :xname ,extension-xname :long ,extension-name :major ,(when major-version (parse-integer major-version)) :minor ,(when minor-version (parse-integer minor-version)))
	    (when minor-version  `(:constant (,(s+ '+ header "-minor" '+) . ,minor-version)))
	    (when major-version  `(:constant (,(s+ '+ header "-major" '+) . ,major-version))))))
(defun parse-xerror (unit)
  "(:xerror (name . code) (fields))"
  (let ((name (car (unit-name unit)))
	(code (parse-integer (fifth (car unit))))
	(fields (parse-fields (cdr unit))))
    `(:xerror (,name  . ,code) ,fields)))

;; xproto glx needs this ;x-shm uses it to copy error from proto
(defun parse-errorcopy (unit ext)
  "Returns :XERROR with fields copied."
  (with-keys (name number ref) (cdar unit)
	 `(:xerror (,name . ,(parse-integer number)) ,@(cddr (find-error ref ext)))))

(defun parse-eventcopy (unit ext) ;;Does anything even use this? xproto
  "Returns :XEVENT with fields copied."
  (with-keys (name number ref) (cdar unit)
    `(:xevent (,name . ,(parse-integer number)) ,@(cddr (find-event ref ext)))))

;;;with fields
(defun parse-struct (unit)
  "(:struct name (slots))"
  `(:struct ,(unit-name unit)
	    ,(parse-fields (cdr unit))))
(defun parse-event (unit ext)
  (with-keys (name number) (cdar unit)
    `(:xevent (,(extensify-name name (getf (x-name ext) :short)) . ,(parse-integer number)) ,(parse-fields (cdr unit)))))

;;;; Big Boys

;;  field        = ((field-info) &optional(field-qualifier)
;;  field-info   = (id (:type | :bytes)
;;  reply        = (:|reply| field1..n)
;;  request-info = (header :name "name" :opcode "OPCODE")

;; (request-info  &optional field1..n &optional reply)  
;;                  
(defun parse-request (unit)
  (multiple-value-bind (send receive) (parse-fields (cdr unit))
    (with-keys (name opcode) (cdar unit)    
      `(:request (,name . ,(parse-integer opcode)) ,send ,receive))))


(defun parse-fields (fields)
  (let ((slots '()))
    (dolist (slot fields)
      (case (car slot)
	(:|doc|) ;(push `(:doc ,(cdr slot)) slots) ;who needs docs?	  
	(:|reply| (return-from parse-fields
		    (values (progn (when (equal "pad" (caar slots)) ;discard trailing pads in send when reply
				     (pop slots))
				   (nreverse slots))
			    (parse-fields (cdr slot))))))
      (when (consp (car slot))
	(let ((id (caar slot))
	      (keys (cdar slot)))
	  (cond				;id
	    ((eql ':|field| id) (with-keys (name type enum altenum mask) keys
				  (if (not (or enum altenum))
				      (if (string= type "BOOL")
					  (push `("boolean" ,name) slots)
					  (push `(,type ,name) slots))
				      (if enum
					  (push `(,type ,name ,enum) slots)
					  (if altenum
					      (push `(,type ,name ,altenum) slots)
					      (push `(,type ,name ,mask) slots)))))) ;altenum xfixes
	    ((eql ':|switch| id) slot)
	    ((eql ':|list| id) (push (parse-list (cadr slot) keys) slots))
	    ((eql ':|pad| id)  (push `("pad" ,(second keys)) slots))))))
    (when (equal "pad" (caar slots)) (pop slots)) ;discard trailing pads, clx doesn't need them, does anybody want them besides C?
    (nreverse slots)))

(defun parse-list (field keys)
  (with-keys (name type) keys
    (if (atom (car field))
	(case (car field)
	  ((:|fieldref| :|value|) `(:list ,type ,name ,(second field)))
	  (otherwise `(:fixme ,(car field))))
	(if (eql :|op| (caar field))
	    `(:list ,type ,name ,(parse-op field))
	    `(:fixme ,(car field))))))

(defun parse-op (op-list)
  `(,(third (first op-list)) ,(if (atom (first (second op-list)))
				  (second (second op-list))
				  (parse-op (second op-list))) ,(second (third op-list))))
