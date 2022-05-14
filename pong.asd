(in-package :cl-user)

(asdf:defsystem "pong"
  :name "pong"
  :author "Richard Lewis <richard@rjlewis.me.uk>"
  :version "0.0.1"
  :maintainer "Richard Lewis <richard@rjlewis.me.uk>"
  :license "Copyright (C) 2022 Richard Lewis"
  :description "The pong game"
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "pong"))
    :serial t))
  :serial t
  :depends-on (:sdl2))
