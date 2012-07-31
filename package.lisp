;;;; package.lisp

(defpackage #:clx-cursor
  (:nicknames #:xcursor)
  (:use #:cl)
  (:export
   :*icon-paths*

   :set-theme
   :theme

   :xcursor
   :make-xcursor
   :xcursor-p
   :copy-xcursor
   :xcursor-filename
   :xcursor-copyright
   :xcursor-license
   :xcursor-other
   :xcursor-images

   :xcursor-image
   :make-xcursor-image
   :xcursor-image-p
   :copy-xcursor-image
   :xcursor-image-width
   :xcursor-image-height
   :xcursor-image-xhot
   :xcursor-image-yhot
   :xcursor-image-delay
   :xcursor-image-data

   :load-cursor-from-file
   :add-cursor
   :cursor
   )
  (:documentation "Library for loading cursor pixmaps from Xcur files and rendering it 
with Xrender. Library supports themes."))

