(in-package :gamebox-ecs)

(defclass trait ()
  ((attrs :accessor %trait-attrs
          :initarg :attrs
          :initform nil)))

(defmacro deftrait (name &body (attrs))
  "Define a new trait."
  (let ((attr-list (mapcar (lambda (x)
                             (intern (format nil "~A/~A" name x)))
                           attrs)))
    `(progn
       (setf (gethash ',name (%traits *ecs*))
             (make-instance 'trait :attrs ',attr-list))
       ,@(loop :for attr :in attr-list
               :for key = (make-keyword attr)
               :collect `(defun ,attr (gob-id)
                           (attr gob-id ,key))
               :collect `(defun (setf ,attr) (attr gob-id)
                           (setf (attr gob-id ,key) attr)))
       ',name)))

(defun %traits-reformat (traits)
  "Helper function to reformat trait data given to %MAKE-GOB."
  (mapcar
   (lambda (trait)
     (loop :with name = (car trait)
           :for (k . v) :in (plist-alist (cdr trait))
           :collect (make-keyword (format nil "~A/~A" name k)) :into slots
           :collect (gensym (symbol-name k)) :into vars
           :collect v :into values
           :finally (return (list name slots (mapcar #'list vars values)))))
   traits))

(defun trait-list ()
  "Get a list of all defined traits."
  (hash-table-keys (%traits *ecs*)))

(defun traitp (trait)
  "Check if a trait is defined."
  (when (member trait (trait-list)) t))

(defun trait-attrs (trait)
  "Get a list of attributes for a trait."
  (%trait-attrs (gethash trait (%traits *ecs*))))

(defun (setf trait-attrs) (attrs trait)
  "Assign a list of attributes to a trait."
  (setf (trait-attrs (gethash trait (%traits *ecs*))) attrs))

(defun trait-attr-add (trait attr)
  "Add a new attribute to a trait."
  (pushnew attr (trait-attrs trait)))

(defun traits (gob-id)
  "Get a list of all traits of a GOB."
  (%gob-traits (gob gob-id)))

(defun (setf traits) (traits gob-id)
  "Assign a list of traits to a GOB."
  (setf (%gob-traits (gob gob-id)) traits))

(defun has-trait-p (gob-id trait)
  "Check if a GOB has a trait."
  (when (member trait (traits gob-id)) t))

(defun trait-add (gob-id trait attrs)
  "Add a new trait to a GOB."
  (when (traitp trait)
    (pushnew trait (traits gob-id))
    (on-trait-added gob-id trait))
  (cache-gobs)
  (loop :for (attr . value) :in (plist-alist attrs)
        :when (member attr (mapcar #'make-keyword (trait-attrs trait)))
          :do (setf (attr gob-id attr) value)))

(defun trait-remove (gob-id trait)
  "Remove a trait from a GOB."
  (when (has-trait-p gob-id trait)
    (deletef (traits gob-id) trait)
    (on-trait-removed gob-id trait)
    (cache-gobs)
    (loop :for attr :in (mapcar #'make-keyword (trait-attrs trait))
          :when (attr gob-id attr)
            :do (attr-remove gob-id attr))))

(slog:define-message :debug :ecs.trair.add
  "GOB ~A now has the trait ~A.")

(defgeneric on-trait-added (gob-id trait)
  (:documentation "Event triggered when a trait is added to a GOB.")
  (:method (gob-id trait))
  (:method :around (gob-id trait)
    (call-next-method)
    (slog:emit :ecs.trait.add gob-id trait)))

(slog:define-message :debug :ecs.trait.remove
  "GOB ~A no longer has the trait ~A.")

(defgeneric on-trait-removed (gob-id trait)
  (:documentation "Event triggered when a trait is removed from a GOB.")
  (:method (gob-id trait))
  (:method :around (gob-id trait)
    (call-next-method)
    (slog:emit :ecs.trait.remove gob-id trait)))
