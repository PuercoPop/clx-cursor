(in-package :clx-cursor)

(defun render-create-anim-cursor (display cursors-delays)
  "Create animated cursor. @var{cursors-delays} should be
sequence #(cursor-id-1 delay-1 cursor-id-2 delay-2 ... )."
  (xlib::ensure-render-initialized display)
  (let* ((cursor (xlib::make-cursor :display display))
         (cid (xlib::allocate-resource-id display cursor 'cursor)))
    (setf (xlib:cursor-id cursor) cid)
    (xlib::with-buffer-request (display (xlib::extension-opcode display "RENDER"))
      (data xlib::+X-RenderCreateAnimCursor+)
      (resource-id cid)
      ((sequence :format xlib:card32) cursors-delays))
    cursor))
