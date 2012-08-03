

(defpackage #:clx-cursor-test
  (:nicknames #:xcursor-test)
  (:use cl)
  (:export :show-window))

(in-package #:clx-cursor-test)

(defun show-window (theme)
  (let* ((display (xlib:open-default-display))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen))
         (black (xlib:screen-black-pixel screen))
         (white (xlib:screen-white-pixel screen))
         (window
           (xlib:create-window :parent root :x 0 :y 0 :width 640 :height 480 
                               :class :input-output
                               :background white
                               :event-mask '(:key-press :key-release :exposure :button-press
                                        :structure-notify)))
         (grackon (xlib:create-gcontext
                   :drawable window
		   :foreground black
		   :background white))
         (font (xlib:open-font display "9x15bold")))
    (unwind-protect
         (progn
           (clx-cursor:set-theme display theme)
           ;;(clx-cursor:add-cursor window (clx-cursor:load-cursor-from-file display "/usr/share/icons/Vanilla-DMZ-LH/cursors/14fef782d02440884392942c11205230") :default)
           (xlib:map-window window)
           (setf (xlib:gcontext-foreground grackon) black)
           (xlib:event-case (display :force-output-p t
                                     :discard-p t)
             (:exposure ()
                        (xlib:clear-area window :width (xlib:drawable-width window)
                                                :height (xlib:drawable-height window))
                        (setf (xlib:gcontext-font grackon) font)
                        (do ((i 0 (+ 2 i))
                             (x 20)
                             (y 20))
                            ((> i 152))
                          (xlib:draw-glyphs window grackon x y
                                            (symbol-name (elt clx-cursor::*cursor-names* 
                                                              (find i clx-cursor::*cursor-names*))))
                          (incf x 200)
                          (when (> x 600)
                            (incf y 20)
                            (setf x 20)))
                        nil)
             (:button-press (code state x y)
                            (if (>= 152 (* 2 (+ (floor (- x 20) 200)
                                                  (* 3 (floor y 20)))))
                                (setf (xlib:window-cursor window)
                                      (clx-cursor:cursor 
                                       window
                                       (elt clx-cursor::*cursor-names* 
                                            (find (* 2 (+ (floor (- x 20) 200)
                                                          (* 3 (floor y 20))))
                                                  clx-cursor::*cursor-names*))))
                                (setf (xlib:window-cursor window)
                                      (clx-cursor:cursor window :X-cursor)))
                            nil
                            )
             (:key-press (code state) (char= #\Space (xlib:keycode->character display code state)))))
      (progn
        (xlib:close-font font)
        (xlib:free-gcontext grackon)
        (xlib:destroy-window window)
        (xlib:close-display display)))))
