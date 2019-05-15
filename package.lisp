(defpackage xparser
  (:use :cl :s-xml))
(in-package :xparser)

(defparameter *current* nil )
(defstruct (extension (:conc-name x-))
  name
  exports
  constants
  types
  enums
  structs
  opcode-fn-name
  opcodes ; (request-name . opcode)
  events
  event-vector
  errors
  error-vector
  requests
  request-vector
  code
  unknown)
