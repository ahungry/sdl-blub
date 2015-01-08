;;;; sdl-blub.asd

(asdf:defsystem #:sdl-blub
  :description "Describe sdl-blub here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx
               #:lispbuilder-sdl-image
               #:lispbuilder-sdl-mixer
               #:cl-opengl
               #:vecto
               #:glyphs)
  :serial t
  :components ((:file "package")
               (:file "sdl-blub")))
