(in-package :box.ecs)

(defun all (list1 list2)
  "Check if all elements of LIST1 are in LIST2."
  (and (intersection list1 list2)
       (not (set-difference list1 list2))))

(defun any (list1 list2)
  "Check if any element of LIST1 is in LIST2."
  (when (intersection list1 list2) t))

(defun none (list1 list2)
  "Check if no element of LIST1 is in LIST2."
  (not (any list1 list2)))

(defun filter (gob-id filter-fn gob-fn items)
  "Filter a GOB."
  (funcall filter-fn items (funcall gob-fn gob-id)))
