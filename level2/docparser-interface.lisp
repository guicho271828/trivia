
(in-package :trivia.level2.impl)

(defclass pattern-node (docparser:operator-node)
     ()
  (:documentation "A pattern-match clause."))

(defmethod print-object ((var pattern-node) stream)
  "Print a pattern node."
  (print-unreadable-object (var stream)
    (format stream "pattern ~A" (docparser:render-humanize (docparser:node-name var)))))

(docparser::define-parser defpattern (name (&rest args) &rest body)
  (let ((docstring (docparser:extract-docstring body)))
    (make-instance 'pattern-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))

(defclass inline-pattern-node (docparser:operator-node)
  ()
  (:documentation "An inline pattern-match clause."))

(defmethod print-object ((var inline-pattern-node) stream)
  "Print a inline-pattern node."
  (print-unreadable-object (var stream)
    (format stream "inline-pattern ~A" (docparser:render-humanize (docparser:node-name var)))))

(docparser::define-parser defpattern-inline (name (&rest args) &rest body)
  (let ((docstring (docparser:extract-docstring body)))
    (make-instance 'inline-pattern-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))

(defclass optimizer-node (docparser:operator-node)
  ()
  (:documentation "A pattern-match optimizer."))

(defmethod print-object ((var optimizer-node) stream)
  "Print a optimizer node."
  (print-unreadable-object (var stream)
    (format stream "optimizer ~A" (docparser:render-humanize (docparser:node-name var)))))

(docparser::define-parser defoptimizer (name (&rest args) &rest body)
  (let ((docstring (docparser:extract-docstring body)))
    (make-instance 'optimizer-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))
