;;;; bookmarks.asd

(asdf:defsystem #:bookmarks
  :description "Describe bookmarks here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mito #:cl-json #:clack #:caveman2 #:djula)
  :components ((:file "package")
               (:file "bookmarks")))
