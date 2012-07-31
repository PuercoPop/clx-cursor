;;;; clx-cursor.asd

(asdf:defsystem #:clx-cursor
  :serial t
  :description "Pure Common Lisp library for antialiased cursor rendering for clx."
  :author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:clx #:cl-fad)
  :components ((:file "package")
               (:file "xrender-patch")
               (:file "clx-cursor")))


(asdf:defsystem #:clx-cursor-test
  :serial t
  :description "Tests for clx-cursor."
  :author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:clx #:cl-fad)
  :components ((:module test
                        :components ((:file "hello-world")))))

