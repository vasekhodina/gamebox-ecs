(in-package :cl-user)

(defpackage #:gamebox-ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:all
           #:any
           #:none
           #:groups
           #:gob
           #:make-gob
           #:remove-gob
           #:on-gob-created
           #:on-gob-deleted
           #:traits
           #:deftrait
           #:trait-add
           #:trait-remove
           #:on-trait-added
           #:on-trait-removed
           #:defbehavior
           #:process-behavior
           #:cycle-behaviors
           #:on-behavior-start
           #:on-behavior-stop))
