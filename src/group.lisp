(in-package :gamebox-ecs)

(defun group-list ()
  "Get a list of all active groups. An active group has at least one member."
  (hash-table-keys (%groups *ecs*)))

(defun groupp (group)
  "Check if a group is active."
  (when (member group (group-list)) t))

(defun %group-members (group)
  "Get a list of GOBs in a group."
  (gethash group (%groups *ecs*)))

(defun (setf %group-members) (members group)
  "Assign a list of GOBS to a group."
  (setf (gethash group (%groups *ecs*)) members))

(defun group-delete (group)
  "Delete a group. Current members of the deleted group will continue to exist,
but will lose their membership. The ON-GROUP-LEAVE event will be triggered for
each group that was left."
  (dolist (gob (%group-members group))
    (group-leave gob group))
  (when (groupp group)
    (remhash group (%groups *ecs*))
    (on-group-deleted group)))

(defun groups (gob-id)
  "Get a list of all groups a GOB is a member of."
  (%gob-groups (gob gob-id)))

(defun gobs-by-group (group)
  "Get a list of GOBs in a group."
  (%group-members group))

(defun group-member-p (gob-id group)
  "Check if a GOB is a member of a specific group."
  (when (member group (groups gob-id)) t))

(defun grouped-p (gob-id)
  "Check if a GOB is a member of any group."
  (when (groups gob-id) t))

(defun group-join (gob-id group)
  "Add a GOB to a group. The ON-GROUP-JOIN event will be triggered in order to
provide custom functionality depending on the group the was joined."
  (unless (group-member-p gob-id group)
    (push group (%gob-groups (gob gob-id)))
    (push gob-id (%group-members group))
    (on-group-join gob-id group)))

(defun group-leave (gob-id group)
  "Remove a GOB from a group. The ON-GROUP-LEAVE event will be triggered in
order to provide custom functionality depending on the group that was left."
  (when (group-member-p gob-id group)
    (deletef (%gob-groups (gob gob-id)) group)
    (deletef (%group-members group) gob-id)
    (on-group-leave gob-id group)
    (unless (%group-members group)
      (group-delete group))))

(defun group-leave-all (gob-id)
  "Remove a GOB from all groups. The ON-GROUP-LEAVE event will be triggered for
each group that was left."
  (dolist (group (groups gob-id))
    (group-leave gob-id group)))

(slog:define-message :debug :ecs.group.join
  "GOB ~A joined group ~A.")

(defgeneric on-group-join (gob-id group)
  (:documentation "Event triggered when a GOB joins a group.")
  (:method (gob-id group))
  (:method :around (gob-id group)
    (slog:emit :ecs.group.join gob-id group)))

(slog:define-message :debug :ecs.group.leave
  "GOB ~A left group ~A.")

(defgeneric on-group-leave (gob-id group)
  (:documentation "Event triggered when a GOB leaves a group.")
  (:method (gob-id group))
  (:method :around (gob-id group)
    (call-next-method)
    (slog:emit :ecs.group.leave gob-id group)))

(slog:define-message :debug :ecs.group.remove
  "Group ~A deleted.")

(defgeneric on-group-deleted (group)
  (:documentation "Event triggered when a group is deleted.")
  (:method (group))
  (:method :around (group)
    (call-next-method)
    (slog:emit :ecs.group.remove group)))
