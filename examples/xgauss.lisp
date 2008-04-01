(eval-when (:compile-toplevel)
  (require 'clx)
  (require 'cl-randist)
  (defpackage #:xgauss
    (:use #:cl #:xlib :randist)
    (:export run)))

(in-package :xgauss)

(defun run (&key (width 400) (height 400)  (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))

	 (points (loop for i from 0 to 10000
		    collect (cons
			     (floor (random-normal (/ width 2) (/ width 10)))
			     (floor (random-normal (/ height 2) (/ height 10))))))
						 
	 (gc (xlib:create-gcontext
		   :drawable root-window
		   :foreground black
		   :background white))

	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background white
		     :event-mask (xlib:make-event-mask :exposure
						       :button-press))))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:exposure ()
		 (dolist (pt points)
		   (xlib:draw-point my-window
				   gc
				   (car pt) (cdr pt))))
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))
