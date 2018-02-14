(in-package :cl-user)

(defpackage #:box.ecs
  (:use #:cl)
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
           #:get-tag
           #:gob-by-tag
           #:tag-add
           #:tag-remove
           #:on-tag-added
           #:on-tag-removed
           #:gob-groups
           #:gobs-by-group
           #:group-join
           #:group-leave
           #:on-group-join
           #:on-group-leave
           #:traits
           #:has-trait-p
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
