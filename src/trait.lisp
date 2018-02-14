(in-package :box.ecs)

(defclass trait ()
  ((id :reader id
       :initarg :id)
   (attrs :accessor %trait-attrs
          :initarg :attrs
          :initform nil)
   (defaults :reader %defaults
             :initform (make-hash-table))))

(defmethod print-object ((object trait) stream)
  (print-unreadable-object (object stream)
    (format stream "TRAIT:~A" (id object))))

(defmacro deftrait (name &body (attrs))
  "Define a new trait."
  (let ((attr-list (mapcar (lambda (x)
                             (let ((attr (alexandria:ensure-list x)))
                               (list
                                (intern (format nil "~A/~A" name (car attr)))
                                (cadr attr))))
                           attrs)))
    `(progn
       (defclass ,name (trait) ())
       (setf (gethash ',name (%traits *ecs*))
             (make-instance ',name :id ',name :attrs ',(mapcar #'car attr-list)))
       (loop :with trait = (gethash ',name (%traits *ecs*))
             :for (attr default) :in ',attr-list
             :do (setf (gethash attr (%defaults trait)) default))
       ,@(loop :for (attr nil) :in attr-list
               :for key = (alexandria:make-keyword attr)
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
           :for (k . v) :in (alexandria:plist-alist (cdr trait))
           :collect (alexandria:make-keyword (format nil "~A/~A" name k)) :into slots
           :collect (gensym (symbol-name k)) :into vars
           :collect v :into values
           :finally (return (list name slots (mapcar #'list vars values)))))
   traits))

(defun trait-list ()
  "Get a list of all defined traits."
  (alexandria:hash-table-keys (%traits *ecs*)))

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

(defun trait-add (gob-id trait-id attrs)
  "Add a new trait to a GOB."
  (when (traitp trait-id)
    (pushnew trait-id (traits gob-id))
    (on-trait-added gob-id trait-id))
  (cache-gobs)
  (loop :with trait = (gethash trait-id (%traits *ecs*))
        :for (attr . default) :in (alexandria:hash-table-alist (%defaults trait))
        :do (setf (attr gob-id (alexandria:make-keyword attr)) (eval default)))
  (loop :for (attr . value) :in (alexandria:plist-alist attrs)
        :when (member attr (mapcar #'alexandria:make-keyword (trait-attrs trait-id)))
          :do (setf (attr gob-id attr) value)))

(defun trait-remove (gob-id trait)
  "Remove a trait from a GOB."
  (when (has-trait-p gob-id trait)
    (deletef (traits gob-id) trait)
    (on-trait-removed gob-id trait)
    (cache-gobs)
    (loop :for attr :in (mapcar #'alexandria:make-keyword (trait-attrs trait))
          :when (attr gob-id attr)
            :do (attr-remove gob-id attr))))

(slog:define-message :debug :ecs.trait.add
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
