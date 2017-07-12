;;
;; SDL OpenGL template
;;

(ql:quickload '(:cl-opengl :sdl2-ttf :sdl2-image))

(defpackage :template
  (:use :cl :cl-opengl :sdl2))

(in-package :template)

(load "utils.lisp")

(defparameter *time* 0.0)
(defparameter *delta* 0.0)
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defun main-screen (delta)
  "Update and render"

  (load-identity)
  
  (incf *time* delta)
  (gl:clear :color-buffer-bit)
  
  (gl:color 1 1 1)
  (text "this is simple sdl+opengl template" 0 0.8 0.05 0)
  
  (let ((gap 0.7)
	(step 0.1))
    
    (gl:color 0.5 0 0)
    (loop for i from (- gap) to gap by step do
	 (loop for j from (- gap) to gap by step do
	      (quad-shape i j (* *time* 200 i j) 0.01 0.04)))

    
    (gl:color 1 1 1 0.2)
    (loop for i from (- gap) to gap by step do
	 (simple-line i gap i (- gap)))))
  
(defun main()
  "Init all stuff and define events"
  (with-init (:everything)
    (sdl2-ttf:init)
    (with-window (window :title "-.-"
			 :w *screen-width*
			 :h *screen-height*
			 :flags '(:shown :resizable :opengl))
      (with-gl-context (gl-context window)
	(gl-make-current window gl-context)	
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(resize-viewport *screen-width* *screen-height*)
	(gl:clear-color 0.1 0.1 0.1 1)

	(with-event-loop (:method :poll)
	  (:keydown     (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-space
			   (print "space"))))
	  (:keyup       (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-escape
			   (push-event :quit))))
	  (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
			t)
	  (:idle        ()
			(idler window))
	  (:windowevent (:event event :data1 width :data2 height)
			(when (=
			       event
			       sdl2-ffi:+sdl-windowevent-size-changed+)
			  (resize-viewport width height)))
	  (:quit        ()
			(sdl2-ttf:quit)
			(clean-text-hash)
			(clean-texture-hash)
			t))))))

(sb-thread:make-thread #'main)


    
