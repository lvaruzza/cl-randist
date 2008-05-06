(eval-when (:compile-toplevel)
  (require 'cl-randist)
  (require 'cl-opengl)
  (require 'lispbuilder-sdl))

(defun gl-gauss ()
  (let  ((points (loop for i from 0 to 10000
		    collect (cons
			     (randist:random-normal 0.5 0.1)
			     (randist:random-normal 0.5 0.1)))))
    
    (sdl:with-init ()
      (sdl:window 400 400
		  :flags sdl-cffi::sdl-opengl
		  :title-caption "Hello Window"
		  :icon-caption "Hello Window")
      (gl:clear-color 0 0 0 0)
      ;; Initialize viewing values.
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (gl:clear :color-buffer-bit)
	       ;; Draw white polygon (rectangle) with corners at
	       ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
	       (gl:color 1 1 1)
	       (gl:with-primitive :points
		 (dolist (pt points)
		   (gl:vertex (car pt) (cdr pt) 0)))
	       ;; Start processing buffered OpenGL routines.
	       (gl:flush)
	       (sdl-cffi::sdl-gl-swap-buffers))))))
