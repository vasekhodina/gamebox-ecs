(in-package :gamebox-ecs)

(defclass gob ()
  ((tag :accessor %gob-tag
        :initform nil)
   (groups :accessor %gob-groups
           :initform nil)
   (traits :accessor %gob-traits
           :initform nil)
   (attrs :accessor %gob-attrs
          :initform nil)))

(defmacro make-gob (tag (&rest groups) &body traits)
  "Make a new GOB."
  (let ((parts (%traits-reformat traits)))
    `(let (,@(mapcan #'third parts))
       (%make-gob :tag ',tag
                 :groups ',groups
                 :traits (list ,@(mapcan
                                  (lambda (part)
                                    (list `(list ',(first part)
                                                 ,@ (mapcan
                                                     (lambda (sym bind)
                                                       `(,sym ,(first bind)))
                                                     (second part)
                                                     (third part)))))
                                  parts))))))

(defun gob (id)
  "Get a GOB by its ID."
  (gethash id (gobs-active *ecs*)))

(defun (setf gob) (gob gob-id)
  "Set the specified ID to a new GOB."
  (setf (gethash gob-id (gobs-active *ecs*)) gob))

(defun gob-list ()
  "Get a list of all active GOBs."
  (hash-table-keys (gobs-active *ecs*)))

(defun %make-gob (&key tag groups traits)
  "Create a new GOB, optionally assigning a unique tag or a list of groups."
  (let ((gob-id (make-gob-id)))
    (setf (gob gob-id) (make-instance 'gob))
    (on-gob-created gob-id)
    (dolist (group groups)
      (group-join gob-id group))
    (tag-add gob-id tag)
    (loop :for (trait . attrs) :in traits
          :do (trait-add gob-id trait attrs))
    (cache-gobs)
    gob-id))

(defun remove-gob (gob-id)
  "Remove a GOB."
  (when (gob gob-id)
    (group-leave-all gob-id)
    (tag-remove gob-id)
    (remhash gob-id (gobs-active *ecs*))
    (push gob-id (gobs-inactive *ecs*))
    (cache-gobs)
    (on-gob-deleted gob-id)))

(slog:define-message :debug :ecs.gob.add
  "GOB ~A created.")

(defgeneric on-gob-created (gob-id)
  (:documentation "Event triggered when a GOB is created.")
  (:method (gob-id))
  (:method :around (gob-id)
    (call-next-method)
    (slog:emit :ecs.gob.add gob-id)))

(slog:define-message :debug :ecs.gob.remove
  "GOB ~A deleted.")

(defgeneric on-gob-deleted (gob-id)
  (:documentation "Event triggered when a GOB is deleted.")
  (:method (gob-id))
  (:method :around (gob-id)
    (call-next-method)
    (slog:emit :ecs.gob.remove gob-id)))
