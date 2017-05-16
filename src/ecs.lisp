(in-package :gamebox-ecs)

(defvar *ecs* nil)

(defclass ecs ()
  ((current-id :accessor current-id
               :initform 0)
   (gobs-active :accessor gobs-active
                :initform (make-hash-table))
   (gobs-inactive :accessor gobs-inactive
                  :initform nil)
   (tags :reader %tags
         :initform (make-hash-table))
   (groups :reader %groups
           :initform (make-hash-table))
   (traits :accessor %traits
           :initform (make-hash-table))
   (behavior :accessor behavior
             :initform (make-hash-table))
   (behavior-order :accessor behavior-order
                   :initform nil)))

(defun init-ecs (&key (log-level :info))
  "Initialize the Ecstasy system, optionally setting the logging level for
printing messages."
  (setf slog:*current-level* log-level
        *ecs* (make-instance 'ecs)))

(defun make-gob-id ()
  "Make a new GOB ID. This will reclaim any IDs from any GOBs that were
removed."
  (or (pop (gobs-inactive *ecs*)) (incf (current-id *ecs*))))
