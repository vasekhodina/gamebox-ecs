(in-package :box.ecs)

(defclass behavior ()
  ((%mode :reader mode
          :initarg :mode)
   (%interval :reader interval
              :initarg :interval)
   (%ticks :accessor ticks
           :initform 0)
   (%filters :reader filters
             :initarg :filters)
   (%grouping :accessor grouping
              :initarg :grouping)
   (%gobs :accessor gobs
          :initform nil)))

(defmacro defbehavior (name (&key mode (interval 1) filters grouping)
                       &body body)
  "Define a new behavior."
  (let ((gobs (gensym)))
    `(progn
       (setf (gethash ',name (behavior *ecs*))
             (make-instance 'behavior
                            :mode ',mode
                            :interval ',interval
                            :filters ',filters
                            :grouping ',grouping))
       (cache-gobs)
       (sort-behaviors)
       (defmethod %process-gobs ((behavior (eql ',name)) &rest ,gobs)
         (block ,name
           (destructuring-bind ,grouping ,gobs
             ,@body))))))

(defun sort-behaviors ()
  "Sort all behaviors in the order they should be processed."
  (setf (behavior-order *ecs*)
        (sort (copy-seq (behavior-list))
              #'string>
              :key (lambda (x) (mode (behavior-by-name x))))))

(defgeneric %process-gobs (behavior &rest gobs))

(defun behavior-by-name (behavior)
  "Get a behavior by name."
  (gethash behavior (behavior *ecs*)))

(defun behavior-list ()
  "Get a list of all defined behaviors."
  (alexandria:hash-table-keys (behavior *ecs*)))

(defun collect-gobs (behavior)
  "Get a list of all GOBs that fall through the specified behavior's filters."
  (loop :with filters = (filters (behavior-by-name behavior))
        :for gob-id :in (alexandria:hash-table-keys (gobs-active *ecs*))
        :when (every #'identity
                     (loop :for (filter fn . items) :in filters
                           :collect (filter gob-id filter fn items)))
          :collect gob-id))

(defun cache-gobs ()
  "Update the the list of GOBs for all behaviors."
  (loop :for behavior :in (behavior-list)
        :for gobs = (collect-gobs behavior)
        :do (setf (gobs (gethash behavior (behavior *ecs*))) gobs)))

(defun process-behavior (behavior)
  "Execute the specified behavior. The behavior definition's grouping determines parallel processing
of GOBs."
  (with-slots (%interval %ticks %grouping %gobs) (behavior-by-name behavior)
    (let ((result))
      (incf %ticks)
      (when (and (>= %ticks %interval)
                 (>= (length %gobs) (length %grouping)))
        (on-behavior-start behavior %gobs)
        (if (= (length %gobs) 1)
            (setf result (apply #'%process-gobs behavior %gobs))
            (alexandria:map-combinations
             (lambda (x) (setf result (apply #'%process-gobs behavior x)))
             %gobs :length (length %grouping)))
        (on-behavior-stop behavior %gobs)
        (setf %ticks 0))
      result)))

(defun cycle-behaviors ()
  "Process all behaviors once."
  (dolist (behavior (behavior-order *ecs*))
    (process-behavior behavior)))

(slog:define-message :debug :ecs.behavior.start
  "Behavior ~A started for ~A GOB~:p")

(defgeneric on-behavior-start (behavior gobs)
  (:documentation "Event triggered when a behavior starts.")
  (:method (behavior gobs))
  (:method :around (behavior gobs)
    (call-next-method)
    (slog:emit :ecs.behavior.start behavior (length gobs))))

(slog:define-message :debug :ecs.behavior.stop
  "Behavior ~A stopped for ~A GOB~:p")

(defgeneric on-behavior-stop (behavior gobs)
  (:documentation "Event triggered when a behavior stops.")
  (:method (behavior gobs))
  (:method :around (behavior gobs)
    (call-next-method)
    (slog:emit :ecs.behavior.stop behavior (length gobs))))
