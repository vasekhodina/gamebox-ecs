(in-package :box.ecs)

(defun attrs (gob-id)
  "Get a list of all of a GOB's attributes."
  (%gob-attrs (gob gob-id)))

(defun (setf attrs) (attrs gob-id)
  "Assign a list of attributes to a GOB."
  (setf (%gob-attrs (gob gob-id)) attrs))

(defun attr (gob-id attr)
  "Get the value of one of a GOB's attributes."
  (getf (attrs gob-id) attr))

(defun (setf attr) (value gob-id attr)
  "Set the value of one of a GOB's attributes."
  (setf (getf (attrs gob-id) attr) value))

(defun attr-remove (gob-id attr)
  "Remove one of a GOB's attributes."
  (delete-from-plistf (attrs gob-id) attr))
