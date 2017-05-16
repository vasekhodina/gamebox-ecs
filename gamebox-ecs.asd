(in-package :cl-user)

(asdf:defsystem #:gamebox-ecs
  :description "An implementation of the Entity-Component System (ECS) pattern, popular with game development."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-ecs"
  :bug-tracker "https://github.com/mfiano/gamebox-ecs/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-ecs.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:simple-logger)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "ecs")
   (:file "gob")
   (:file "group")
   (:file "tag")
   (:file "attr")
   (:file "trait")
   (:file "behavior")))
