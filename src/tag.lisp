(in-package :box.ecs)

(defun tag-list ()
  "Get a list of all active tags. An active tag is a tag currently assigned to a GOB."
  (alexandria:hash-table-keys (%tags *ecs*)))

(defun tagp (tag)
  "Check if a tag is active."
  (when (member tag (tag-list)) t))

(defun %tag-owner (tag)
  "Get the GOB with the specified tag."
  (gethash tag (%tags *ecs*)))

(defun (setf %tag-owner) (gob-id tag)
  "Assign a tag to a GOB."
  (setf (gethash tag (%tags *ecs*)) gob-id))

(defun tag-delete (tag)
  "Delete a tag. A GOB that has this tag assigned will continue to exist, but will lose its tag. The
ON-TAG-REMOVED event will be triggered for the deleted tag."
  (tag-remove (gob-by-tag tag))
  (remhash tag (%tags *ecs*)))

(defun get-tag (gob-id)
  "Get the tag of a GOB."
  (%gob-tag (gob gob-id)))

(defun gob-by-tag (tag)
  "Get the owner with the specified tag."
  (%tag-owner tag))

(defun has-tag-p (gob-id tag)
  "Check if a GOB has a specific tag."
  (when (eq tag (get-tag gob-id)) t))

(defun tagged-p (gob-id)
  "Check if a GOB has any tag."
  (when (get-tag (gob gob-id)) t))

(defun tag-add (gob-id tag)
  "Add a tag to a GOB. If the specified GOB already has a different tag, it will be lost. If another
GOB already has this tag, it will be lost. The ON-TAG-ADDED event will be triggered in order to
provide custom functionality depending on the tag added."
  (alexandria:when-let ((previous-tag (get-tag gob-id)))
    (tag-delete previous-tag))
  (alexandria:when-let ((previous-gob (gob-by-tag tag)))
    (tag-remove previous-gob))
  (unless (has-tag-p gob-id tag)
    (setf (%gob-tag (gob gob-id)) tag
          (%tag-owner tag) gob-id)
    (on-tag-added gob-id tag)))

(defun tag-remove (gob-id)
  "Remove a tag from a GOB. The ON-TAG-REMOVED event will be triggered in order to provide custom
functionality depending on the tag removed."
  (alexandria:when-let ((tag (get-tag gob-id)))
    (setf (%gob-tag (gob gob-id)) nil)
    (remhash tag (%tags *ecs*))
    (on-tag-removed gob-id tag)))

(simple-logger:define-message :debug :ecs.tag.add
  "GOB ~A tagged as ~A.")

(defgeneric on-tag-added (gob-id tag)
  (:documentation "Event called when a GOB is tagged.")
  (:method (gob-id tag))
  (:method :around (gob-id tag)
    (call-next-method)
    (simple-logger:emit :ecs.tag.add gob-id tag)))

(simple-logger:define-message :debug :ecs.tag.remove
  "GOB ~A untagged as ~A.")

(defgeneric on-tag-removed (gob-id tag)
  (:documentation "Event called when a GOB is untagged.")
  (:method (gob-id tag))
  (:method :around (gob-id tag)
    (call-next-method)
    (simple-logger:emit :ecs.tag.remove gob-id tag)))
