;;;; clx-cursor.lisp

(in-package #:clx-cursor)

(defvar cursor-font "cursor")

(defvar num-glyphs 154)

(defvar X-cursor 0)
(defvar arrow 2)
(defvar based-arrow-down 4)
(defvar based-arrow-up 6)
(defvar boat 8)
(defvar bogosity 10)
(defvar bottom-left-corner 12)
(defvar bottom-right-corner 14)
(defvar bottom-side 16)
(defvar bottom-tee 18)
(defvar box-spiral 20)
(defvar center-ptr 22)
(defvar circle 24)
(defvar clock 26)
(defvar coffee-mug 28)
(defvar cross 30)
(defvar cross-reverse 32)
(defvar crosshair 34)
(defvar diamond-cross 36)
(defvar dot 38)
(defvar dotbox 40)
(defvar double-arrow 42)
(defvar draft-large 44)
(defvar draft-small 46)
(defvar draped-box 48)
(defvar exchange 50)
(defvar fleur 52)
(defvar gobbler 54)
(defvar gumby 56)
(defvar hand1 58)
(defvar hand2 60)
(defvar heart 62)
(defvar icon 64)
(defvar iron-cross 66)
(defvar left-ptr 68)
(defvar left-side 70)
(defvar left-tee 72)
(defvar leftbutton 74)
(defvar ll-angle 76)
(defvar lr-angle 78)
(defvar man 80)
(defvar middlebutton 82)
(defvar mouse 84)
(defvar pencil 86)
(defvar pirate 88)
(defvar plus 90)
(defvar question-arrow 92)
(defvar right-ptr 94)
(defvar right-side 96)
(defvar right-tee 98)
(defvar rightbutton 100)
(defvar rtl-logo 102)
(defvar sailboat 104)
(defvar sb-down-arrow 106)
(defvar sb-h-double-arrow 108)
(defvar sb-left-arrow 110)
(defvar sb-right-arrow 112)
(defvar sb-up-arrow 114)
(defvar sb-v-double-arrow 116)
(defvar shuttle 118)
(defvar sizing 120)
(defvar spider 122)
(defvar spraycan 124)
(defvar star 126)
(defvar target 128)
(defvar tcross 130)
(defvar top-left-arrow 132)
(defvar top-left-corner 134)
(defvar top-right-corner 136)
(defvar top-side 138)
(defvar top-tee 140)
(defvar trek 142)
(defvar ul-angle 144)
(defvar umbrella 146)
(defvar ur-angle 148)
(defvar watch 150)
(defvar xterm 152)

(defparameter *cursor-names*
  (list
   :X-cursor 0
   :arrow 2
   :based-arrow-down 4
   :based-arrow-up 6
   :boat 8
   :bogosity 10
   :bottom-left-corner 12
   :bottom-right-corner 14
   :bottom-side 16
   :bottom-tee 18
   :box-spiral 20
   :center-ptr 22
   :circle 24 
   :clock 26
   :coffee-mug 28 
   :cross 30
   :cross-reverse 32
   :crosshair 34
   :diamond-cross 36
   :dot 38 
   :dotbox 40
   :double-arrow 42
   :draft-large 44 
   :draft-small 46
   :draped-box 48
   :exchange 50
   :fleur 52
   :gobbler 54 
   :gumby 56
   :hand1 58
   :hand2 60
   :heart 62
   :icon 64
   :iron-cross 66
   :left-ptr 68
   :left-side 70
   :left-tee 72
   :leftbutton 74
   :ll-angle 76
   :lr-angle 78
   :man 80
   :middlebutton 82
   :mouse 84
   :pencil 86
   :pirate 88
   :plus 90
   :question-arrow 92
   :right-ptr 94
   :right-side 96
   :right-tee 98
   :rightbutton 100
   :rtl-logo 102
   :sailboat 104
   :sb-down-arrow 106
   :sb-h-double-arrow 108
   :sb-left-arrow 110
   :sb-right-arrow 112
   :sb-up-arrow 114
   :sb-v-double-arrow 116
   :shuttle 118
   :sizing 120
   :spider 122
   :spraycan 124
   :star 126
   :target 128
   :tcross 130
   :top-left-arrow 132
   :top-left-corner 134
   :top-right-corner 136
   :top-side 138
   :top-tee 140
   :trek 142
   :ul-angle 144
   :umbrella 146
   :ur-angle 148
   :watch 150
   :xterm 152))

(defun get-old-cursor (drawable name)
  "Returns x11 standard cursor glyph"
  (let* ((font (xlib:open-font (xlib:drawable-display drawable) cursor-font))
         (cursor (xlib:create-glyph-cursor :source-font font 
                                           :mask-font font
                                           :source-char name
                                           :mask-char (1+ name)
                                           :foreground (xlib:make-color :red 0 :green 0 :blue 0)
                                           :background (xlib:make-color))))
    cursor))

;;; Xresources
(defun friendly-get-resource (database path &optional default)
  "Get resource value from @var{database} and @{path}. If value is not found, returns default."
  (xlib:map-resource database (lambda (it-path value)
                                (when (equal it-path path)
                                  (return-from friendly-get-resource value))))
  default)

(defparameter resource-db (xlib:make-resource-database))

(when (fad:file-exists-p (merge-pathnames 
                          ".Xdefaults" (user-homedir-pathname)))
  (xlib:read-resources resource-db (merge-pathnames 
                                    ".Xdefaults" (user-homedir-pathname))))

(when (fad:file-exists-p (merge-pathnames ".Xresources"
                                          (user-homedir-pathname)))
  (xlib:read-resources resource-db (merge-pathnames ".Xresources"
                                                    (user-homedir-pathname))))

(defparameter Xcursor.theme (friendly-get-resource resource-db '("Xcursor" "theme") "default")
  "Contains default theme name. Readed from Xresources.")
(defparameter Xcursor.size (friendly-get-resource resource-db '("Xcursor" "size") 24)
  "Contains default cursor size. Readed from Xresources.")

(defun set-theme (display &optional (name Xcursor.theme) (size Xcursor.size))
  "Sets current theme to name and size."
  (setf (getf (xlib:display-plist display) :xcursor-theme) name)
  (setf (getf (xlib:display-plist display) :xcursor-size) size)
  (cache-cursor-icons display))

(defun theme (display)
  "Returns current theme name and size."
  (values (getf (xlib:display-plist display) :xcursor-theme)
          (getf (xlib:display-plist display) :xcursor-size)))

(defstruct xcursor
  "Structure contains xcursor information from Xcur file.
Images slot is array of xcursor-images."
  (filename "" :type string)
  (copyright "" :type string)
  (license "" :type string)
  (other (list) :type list)
  (images (make-array '(0) :element-type '(or xcursor-image null) :initial-element nil :adjustable t
                      :fill-pointer 0) :type (array (or xcursor-image null))))

(defstruct xcursor-image
  "Structure containg cursor glyph information. Data is 2 dimensional array with ARGB pixels.
Data can be used in xlib:create-image :data parameter.
Delay slot is used for animated cursors (milliseconds)."
  (width 0 :type xlib:card32)
  (height 0 :type xlib:card32)
  (xhot 0 :type xlib:card32)
  (yhot 0 :type xlib:card32)
  (delay 0 :type xlib:card32)
  (data))

;; magic: CARD32 'Xcur' (0x58, 0x63, 0x75, 0x72)
;; header: CARD32 bytes in this header
;; version: CARD32 file version number
;; ntoc: CARD32 number of toc entries
;; toc: LISTofTOC table of contents 
(defstruct xcursor-header
  "Main file header"
  (magic "" :type string)
  (header 0 :type xlib:card32)
  (version 0 :type xlib:card32)
  (ntoc 0 :type xlib:card32)
  (tc (list ) :type list))

;; type: CARD32 entry type
;; subtype: CARD32 type-specific label - size for images
;; position: CARD32 absolute byte position of table in file 
(defstruct xcursor-toc-header
  "Table of contents"
  (type 0 :type xlib:card32)
  (subtype 0 :type xlib:card32)
  (position 0 :type xlib:card32))

;; header: CARD32 bytes in chunk header (including type-specific fields)
;; type: CARD32 must match type in TOC for this chunk
;; subtype: CARD32 must match subtype in TOC for this chunk
;; version: CARD32 version number for this chunk type 
(defstruct xcursor-chunk
  "Parent structure for chunks."
  (header 0 :type xlib:card32)
  (type 0 :type xlib:card32)
  (subtype 0 :type xlib:card32)
  (version 0 :type xlib:card32))

;; header: 20 Comment headers are 20 bytes
;; type: 0xfffe0001 Comment type is 0xfffe0001
;; subtype: { 1 (COPYRIGHT), 2 (LICENSE), 3 (OTHER) }
;; version: 1
;; length: CARD32 byte length of UTF-8 string
;; string: LISTofCARD8 UTF-8 string 
(defstruct (xcursor-comment-chunk (:include xcursor-chunk))
  "Structure for comment chunks."
  (length 0 :type xlib:card32)
  (string "" :type string))

;; header: 36 Image headers are 36 bytes
;; type: 0xfffd0002 Image type is 0xfffd0002
;; subtype: CARD32 Image subtype is the nominal size
;; version: 1
;; width: CARD32 Must be less than or equal to 0x7fff
;; height: CARD32 Must be less than or equal to 0x7fff
;; xhot: CARD32 Must be less than or equal to width
;; yhot: CARD32 Must be less than or equal to height
;; delay: CARD32 Delay between animation frames in milliseconds
;; pixels: LISTofCARD32 Packed ARGB format pixels 
(defstruct (xcursor-image-chunk (:include xcursor-chunk))
  "Structure for image chunks."
  (width 0 :type xlib:card32)
  (height 0 :type xlib:card32)
  (xhot 0 :type xlib:card32)
  (yhot 0 :type xlib:card32)
  (delay 0 :type xlib:card32)
  (pixels (make-array '(1 1) :element-type 'xlib:card32)
   :type (array xlib:card32 (* *))))

(defun read-reverted-card32 (stream)
  (let ((u32 0))
    (declare (type xlib:card32 u32))
    (setf (ldb (byte 8 24) u32) (read-byte stream))
    (setf (ldb (byte 8 16) u32) (read-byte stream))
    (setf (ldb (byte 8 8) u32) (read-byte stream))
    (setf (ldb (byte 8 0) u32) (read-byte stream))
    u32))

(defun read-card32 (stream)
  (let ((u32 0))
    (declare (type xlib:card32 u32))
    (setf (ldb (byte 8 0) u32) (read-byte stream))
    (setf (ldb (byte 8 8) u32) (read-byte stream))
    (setf (ldb (byte 8 16) u32) (read-byte stream))
    (setf (ldb (byte 8 24) u32) (read-byte stream))
    u32))

(defun read-ascii (stream n)
  (with-output-to-string (string)
    (dotimes (i n)
      (write-char (code-char (read-byte stream)) string))))

(defun read-xcursor-toc-header (stream)
  "Reads table of contents"
  (make-xcursor-toc-header 
   :type (read-card32 stream)
   :subtype (read-card32 stream)
   :position (read-card32 stream)))

(defun read-xcursor-comment-chunk (stream header type)
  "Reads comment chunk from stream."
  (let* ((subtype (read-card32 stream))
         (version (read-card32 stream))
         (length (read-card32 stream))
         (comment (read-ascii stream length)))
    (make-xcursor-comment-chunk
     :header header
     :type type
     :subtype subtype
     :version version
     :length length
     :string comment)))

(defun read-xcursor-image-chunk (stream header type)
  "Reads image chunk from stream."
  (let* ((subtype (read-card32 stream))
         (version (read-card32 stream))
         (width (read-card32 stream))
         (height (read-card32 stream))
         (xhot (read-card32 stream))
         (yhot (read-card32 stream))
         (delay (read-card32 stream))
         (chunk
           (make-xcursor-image-chunk
            :header header
            :type type
            :subtype subtype
            :version version
            :width width
            :height height
            :xhot xhot
            :yhot yhot
            :delay delay
            :pixels (make-array (list width height) :element-type 'xlib:card32))))
    (do ((i 0 (1+ i)))
        ((>= i width) chunk)
      (do ((j 0 (1+ j)))
          ((>= j height))
        (let ((u32 (read-card32 stream)))
          ;;(setf (ldb (byte 8 8) u32) #xFF)
          (setf (aref (xcursor-image-chunk-pixels chunk) i j) u32))))))

(defun read-xcursor-chunk (stream)
  "Reads chunk."
  (let* ((header (read-card32 stream))
         (type (read-card32 stream)))
    (cond
      ((= type #xfffe0001)
       (read-xcursor-comment-chunk stream header type))
      ((= type #xfffd0002)
       (read-xcursor-image-chunk stream header type)))))

(defun load-cursor-from-file (display filepath)
  "Reads Xcur file, and returns xcursor object. xcursor object can be used in @reffun{add-cursor}."
  (with-open-file (filestream filepath
                              :element-type '(unsigned-byte 8)
                              :if-does-not-exist :error)
    (let ((signature (read-ascii filestream 4))
          (xcursor (make-xcursor :filename (pathname-name filepath))))
      (when (string= "Xcur" signature)
        (let ((header (make-xcursor-header
                       :magic signature
                       :header (read-card32 filestream)
                       :version (read-card32 filestream)
                       :ntoc (read-card32 filestream))))
          (dotimes (i (xcursor-header-ntoc header))
            (push (read-xcursor-toc-header filestream)
                  (xcursor-header-tc header)))
          (setf (xcursor-header-tc header) (nreverse (xcursor-header-tc header)))
          (dolist (header (xcursor-header-tc header))
            (file-position filestream (xcursor-toc-header-position header))
            (let ((chunk (read-xcursor-chunk filestream)))
              (typecase chunk
                  (xcursor-image-chunk
                   (when (and (= (xcursor-image-chunk-width chunk)
                                 (getf (xlib:display-plist display) :xcursor-size)
                                 )
                              (= (xcursor-image-chunk-height chunk)
                                 (getf (xlib:display-plist display) :xcursor-size)
                                 ))
                     (vector-push-extend 
                      (make-xcursor-image
                       :width (xcursor-image-chunk-width chunk)
                       :height (xcursor-image-chunk-height chunk)
                       :xhot (xcursor-image-chunk-xhot chunk)
                       :yhot (xcursor-image-chunk-yhot chunk)
                       :delay (xcursor-image-chunk-delay chunk)
                       :data (xcursor-image-chunk-pixels chunk))
                      (xcursor-images xcursor))))
                  (xcursor-comment-chunk 
                   (cond
                     ((= (xcursor-comment-chunk-subtype chunk) 1)
                      (setf (xcursor-copyright xcursor) (xcursor-comment-chunk-string chunk)))
                     ((= (xcursor-comment-chunk-subtype chunk) 2)
                      (setf (xcursor-license xcursor) (xcursor-comment-chunk-string chunk)))
                     (t
                      (push (xcursor-comment-chunk-string chunk) (xcursor-other xcursor)))))))))
        (setf (xcursor-other xcursor)
              (nreverse (xcursor-other xcursor)))
        xcursor))))


;;;; paths for themes
;;;; ~/.icons, /usr/share/icons, /usr/share/pixmaps

(defun display-cursor-cache (display)
  "Returns cursor cache for specified display."
  (or (getf (xlib:display-plist display) :xcursor-cache)
      (setf (getf (xlib:display-plist display) :xcursor-cache)
            (make-hash-table))))

(defparameter *icon-paths* (list
                      (merge-pathnames ".icons/" (user-homedir-pathname))
                      #p"/usr/share/icons/"
                      #p"/usr/share/pixmaps/")
  "Contains paths for themes.")

;;; symbol-string utils
(defun cursor-keyword-to-name (keyword-symbol)
  "Converts keyword to string. Downcases characters, replaces - to _"
  (map 'string
       (lambda (character)
         (if (char= character #\-)
             #\_
             (char-downcase character)))
       (symbol-name keyword-symbol)))

(defun cursor-name-to-keyword (cursor-name)
  "Converts string to keyword. Upcases characters, replaces _ to -"
  (intern
   (map 'string
        (lambda (character)
          (if (char= character #\_)
              #\-
              (char-upcase character)))
        cursor-name)
   (find-package "KEYWORD")))

(defun cursor-name-to-symbol (cursor-name)
  "Converts string to symbol. Upcases characters, replaces _ to -"
  (intern
   (map 'string
        (lambda (character)
          (if (char= character #\_)
              #\-
              (char-upcase character)))
        cursor-name)
   (find-package :clx-cursor)))

(defun keyword-to-symbol (cursor-symbol)
  "Converts keyword to symbol of current package."
  (intern
   (symbol-name cursor-symbol)
   (find-package :clx-cursor)))

(defun clear-cursor-cache (display)
  "Clears cursor cache."
  (let ((cache (display-cursor-cache display)))
    (maphash
     (lambda (key value)
       (declare (ignorable key))
       (typecase value
         (xlib:cursor (ignore-errors (xlib:free-cursor value))))) cache)
    (clrhash cache)))

(defun cache-cursor-icons (display)
  "Caches cursor glyphs from current theme."
  (clear-cursor-cache display)
  (dolist (path (reverse *icon-paths*))
    (dolist (filepath (fad:list-directory 
                       (merge-pathnames 
                        "cursors"
                        (merge-pathnames 
                         (concatenate
                          'string
                          (getf (xlib:display-plist display) :xcursor-theme)
                          "/") path))))
      (setf
       (gethash 
        (cursor-name-to-keyword (pathname-name filepath))
        (display-cursor-cache display))
       filepath))))

(defun get-new-cursor (drawable xcursor)
  "Returns xlib:cursor object for xcursor object. If xcursor object
 does not contain specified width and height, returns standard cursor glyph.
@var{drawable} must be window or pixmap."
  (let* ((image-vector (xcursor-images xcursor))
         (image-count (length image-vector)))
    (cond
      ((= 0 image-count)
       (get-old-cursor drawable (getf *cursor-names* 
                                      (cursor-name-to-keyword (xcursor-filename xcursor))
                                      (getf *cursor-names* :X-cursor))))
      ((= 1 image-count)
       (get-xlib-cursor-object drawable (elt image-vector 0)))
      ((< 1 image-count)
       (let ((cursors-delays (list)))
         (dotimes (i image-count)
           (let* ((image (elt image-vector i))
                  (cursor (get-xlib-cursor-object drawable image))
                  (delay (xcursor-image-delay image)))
             (push (xlib:cursor-id cursor) cursors-delays)
             (push delay cursors-delays)))
         (render-create-anim-cursor (xlib:drawable-display drawable)
                                    (nreverse cursors-delays)))))))

(defun find-argb32-picture-format (display)
  (dolist (format (xlib:find-matching-picture-formats
                   display
                   :depth 32
                   :alpha 8
                   :red 8
                   :green 8
                   :blue 8))
    (when (= 24 (cdr (xlib:picture-format-alpha-byte format)))
      (return-from find-argb32-picture-format format))))

(defun get-xlib-cursor-object (drawable xcursor-image)
  "Returns xlib:cursor object for specified image. Uses Xrender for antialiasing cursor glyph."
  (let* ((curimage xcursor-image)
         (width (xcursor-image-width curimage))
         (height (xcursor-image-height curimage))
         (image (xlib:create-image :width width :height height
                                   :depth 32
                                   :data (xcursor-image-data curimage)
                                   :x-hot (xcursor-image-xhot curimage)
                                   :y-hot (xcursor-image-yhot curimage)
                                   :bits-per-pixel 32))
         (pixmap (xlib:create-pixmap :width width :height height :depth 32
                                     :drawable drawable))
         (gc (xlib:create-gcontext :drawable pixmap))
         (pict (xlib:render-create-picture pixmap
                                           :format (find-argb32-picture-format 
                                                    (xlib:drawable-display drawable)))))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width width :height height)
    (let ((cursor (xlib::render-create-cursor
                   pict
                   (xcursor-image-xhot curimage)
                   (xcursor-image-yhot curimage))))
      (xlib:render-free-picture pict)
      (xlib:free-gcontext gc)
      (xlib:free-pixmap pixmap)
      (xlib:display-finish-output (xlib:drawable-display drawable))
      cursor)))

(defun cursor (drawable name)
  "Returns cursor object, which can be used with (xlib:window-cursor).
If Xrender and theme icon file is available, tries to load cursor from that file, else load standard xlib cursor glyph. index.theme is not supported."
  (let* ((cursor-cache (display-cursor-cache (xlib:drawable-display drawable)))
         (cursor (gethash name cursor-cache)))
    (typecase cursor
      (pathname
       (setf (gethash name cursor-cache)
             (if (null (xlib:query-extension (xlib:drawable-display drawable) "RENDER"))
                 (get-old-cursor drawable (getf *cursor-names* name (getf *cursor-names* :X-cursor)))
                 (get-new-cursor drawable
                                 (load-cursor-from-file (xlib:drawable-display drawable) cursor)))))
      (null (setf (gethash name cursor-cache)
                  (get-old-cursor drawable (getf *cursor-names* name (getf *cursor-names* :X-cursor)))))
      (xlib:cursor cursor))))

(defun add-cursor (dispaly xcursor name)
  "Stores xcursor into cache. @var{name} must be keyword."
  (let* ((cursor-cache (display-cursor-cache display))
        (old-cursor (gethash name cursor-cache)))
    (typecase old-cursor
      (xlib:cursor (xlib:free-cursor old-cursor)))
    (setf (gethash name cursor-cache) (get-new-cursor drawable xcursor))))

;;; "clx-cursor" goes here. Hacks and glory await!
