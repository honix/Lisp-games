;;
;; Visual List Editor
;;

(ql:quickload '(:cl-opengl :sdl2-ttf :sdl2-image))

(defpackage :vle
  (:use :cl :cl-opengl :sdl2))

(in-package :vle)

(load (merge-pathnames "utils.lisp"))

(defstruct node
  name
  x y
  in-focus
  selected
  message
  error
  parents
  childs)

;; main
(defparameter *time* 0.0)
(defparameter *delta* 0.0)
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
;; travel
(defparameter *position* (list 0 0))
(defparameter *real-position* (list 0 0))
(defparameter *zoom* 1.0)
(defparameter *real-zoom* 1.0)
;; *nodes* visual
(defparameter *node-height* 0.06)
(defparameter *node-width-char* 0.021) ;?
(defparameter *node-width-bumps* 0.05)
;; *nodes*
(defparameter *new-node-name* "")
(defparameter *nodes* ())
;(defparameter *in-focus-nodes* ())
;; inputs
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *key-move* nil)
(defparameter *mouse-left* nil)
(defparameter *mouse-right* nil)

;;
;;
;;

(defun draw-wires (node)
  (with-slots (name x y childs mouse-at) node
    (let* ((pulse-in  (/ (mod (+ *time* y) 6) 6))
	   (pulse-out (min 1 (* pulse-in 1.3))))
      (dolist (child childs)
	(gl:color 0.5 0.5 0.5)
	(simple-line x y (node-x child) (node-y child))
	(gl:color 0 1 1)
	(simple-line
	 (+ (node-x child) (* (- x (node-x child)) pulse-in))
	 (+ (node-y child) (* (- y (node-y child)) pulse-in))
	 (+ (node-x child) (* (- x (node-x child)) pulse-out))
	 (+ (node-y child) (* (- y (node-y child)) pulse-out)))))))

(defparameter err nil)

(defun draw-node (node)
  (with-slots (name x y in-focus selected message error parents) node
    (gl:push-matrix)
    (gl:translate x y 0)
    (cond
      (in-focus       (gl:color 0   0.2 0.5))
      ((null parents) (gl:color 0.5 0.0 0.5))
      (t              (gl:color 0.2 0.2 0.2)))
    (quad-shape 0 0 0
		(+ (* (length name) *node-width-char*)
		   *node-width-bumps*)
		*node-height*)
    (when selected
      (gl:color 0 1 1)
      (quad-lines 0 0 0
		  (+ (+ (* (length name) *node-width-char*)
		     *node-width-bumps*) 0.02)
		  (+ *node-height* 0.02)))
    (gl:color 1 1 1)
    (text name 0 0 0.04 0)
    (when message
      (if error
	  (gl:color 1 1 0 0.5)	
	  (gl:color 0 1 1))
      (text message 0 0.1 0.04 0))
    (gl:pop-matrix)))

(defun mouse-at-node-p (node)
  (with-slots (name x y) node
    (let ((half-width  (+ (* (length name) *node-width-char*)
			  *node-width-bumps*))
	  (half-height *node-height*))
      (and (< (- x half-width)  *mouse-x* (+ x half-width))
	   (< (- y half-height) *mouse-y* (+ y half-height))))))

(defun make-connection (parent child)
  (when (not (eq parent child))
    (pushnew child
	     (node-childs parent))
    (pushnew parent
	     (node-parents child))
    (when (node-message child)
      (setf (node-message child) nil))
    (dolist (head (find-heads child))
      (when (node-message head)
	(setf (node-message head) "?")))))

(defun destroy-connections (node)
  (mapc (lambda (parent) (setf (node-childs parent)
			       (remove node (node-childs parent))))
	(node-parents node))
  (setf (node-parents node) ())
  
  (mapc (lambda (child) (setf (node-parents child)
			       (remove node (node-parents child))))
	(node-childs node))
  (setf (node-childs node) ()))

(defun insert-new-node ()
  (when (not (string= *new-node-name* ""))
    (let ((new-node (make-node :name *new-node-name*
			       :x (first *position*)
			       :y (second *position*))))
      (push new-node *nodes*)
      (setf *new-node-name* "")
      (let ((selected-nodes (remove-if-not #'node-selected *nodes*)))
	(cond ((< (length selected-nodes) 1)
	       (setf (node-selected new-node) t)
	       (incf (second *position*) -0.2))
	      ((= (length selected-nodes) 1)
	       (make-connection (car selected-nodes) new-node)
	       (incf (first *position*) 0.2)))))))

(defun find-heads (node)
  "Find all tree-heads linked to node"
  (labels ((find-head-in (node)
	     (if (node-parents node)
		 (mapcar #'find-head-in (node-parents node))
		 node)))
    (remove-duplicates (flatten (find-head-in node)))))

(defun compose-code (node)
  "Make lisp form"
  (with-slots (name parents childs) node
    (if (string= name " ")               ; (child1 child2 ...)
	`(,@(mapcar #'compose-code
		    (sort childs #'< :key #'node-x)))
	(let ((symbol (read-from-string name)))
	  (cond
	    (childs                      ; (symbol child1 child2 ...)
	     `(,symbol
	       ,@(mapcar #'compose-code
			 (sort childs #'< :key #'node-x))))
	    ((and (null parents)         ; (function-symbol)
		  (ignore-errors (symbol-function symbol)))
	     (list symbol))
	    (t                           ; symbol
	     symbol))))))
      
(defun eval-tree (node)
  (mapc (lambda (node)
	  (with-slots (message error) node
	    (let ((cant
		   (nth-value 1 (ignore-errors
				  (let ((result
					 (write-to-string
					  (eval (compose-code node)))))
				    (setf error nil)
				    (setf message result))))))
	      (when cant
		(setf error t)
		(setf message (substitute #\Space #\Linefeed
					  (princ-to-string cant)))))))
	  (find-heads node)))

(eval (list (read-from-string "+") 
	    (read-from-string "2")
	    (read-from-string "5")))
;;
;;
;;

(defun set-mouse-position ()
  (setf *mouse-x* (first *position*)
	*mouse-y* (second *position*)))

(defun main-screen (delta)
  "Update and render"
  ; update
  (incf *time* delta)

  (when *key-move*
    (set-mouse-position))
  
  (if *mouse-left*
      (let ((node (find-if #'node-in-focus *nodes*)))
	(when node
	  (incf (node-x node) (lerp *mouse-x* (node-x node) 0.3))
	  (incf (node-y node) (lerp *mouse-y* (node-y node) 0.3))
	  (let ((new-child (find-if #'mouse-at-node-p (remove node *nodes*))))
	    (when new-child
	      (make-connection node new-child)))))
      (dolist (node *nodes*)
	(setf (node-in-focus node) (mouse-at-node-p node))))
  
  ; draw
  (gl:clear :color-buffer-bit)
  (gl:scale (incf *real-zoom* (lerp *zoom* *real-zoom* 0.3))
	    *real-zoom* 0)
  (gl:translate (- (incf (first *real-position*)
			 (lerp (first *position*)
			       (first *real-position*) 0.3)))
		(- (incf (second *real-position*)
			 (lerp (second *position*)
			       (second *real-position*)0.3)))
		0)

  ; cross
  (gl:color 0.2 0.2 0.2)
  (simple-cross (first *position*) (second *position*) 0.1)

  ; *nodes*
  (mapc #'draw-wires *nodes*)
  (mapc #'draw-node *nodes*)
  
  ; gui
  (gl:load-identity)
  (gl:color 1 1 1)
  (text (if (string= *new-node-name* "")
	    "enter symbol"
	    *new-node-name*)
	0 -0.8 0.03 0)
  (text "|" 0 -0.9 0.03 (* *time* 12))
  (text "|" 0 -0.9 0.03 (* *time* 42)))

(defun press-mouse-left ()
  (setf *mouse-left* t)
  (mapc (lambda (node sel)
	  (setf (node-selected node) sel))
	*nodes*
	(mapcar #'mouse-at-node-p *nodes*)))

(defun main()
  "Init all stuff and define events"
  (with-init (:everything)
    (sdl2-ttf:init)
    (sdl2:gl-set-attr :multisamplebuffers 1) 
    (sdl2:gl-set-attr :multisamplesamples 2) 
    (with-window (window :title "VLE"
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
	  (:textinput   (:text text)
			(let ((char (code-char text)))
			  ;(when (not (char= char #\Space))
			  (setf *new-node-name*
				(concatenate 'string
					     *new-node-name*
					     (list char)))))
	  (:keydown     (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-tab
			   (mapc #'eval-tree
				 (remove-if-not #'node-selected *nodes*)))
			  (:scancode-return
			   (insert-new-node))
			  (:scancode-backspace
			   (setf *new-node-name*
				 (subseq *new-node-name* 0
					 (- (length *new-node-name*) 1))))
			  (:scancode-delete 
			   (mapc #'destroy-connections
				 (remove-if-not #'node-selected *nodes*)))
			  (:scancode-f11
			   (full-screen window))
			  
			  (:scancode-lctrl
			   (setf *mouse-right* t))
			  (:scancode-lalt
			   (press-mouse-left))
			  (:scancode-kp-4
			   (setf *key-move* t)
			   (incf (first *position*) -0.2))
			  (:scancode-kp-6
			   (setf *key-move* t)
			   (incf (first *position*) 0.2))
			  (:scancode-kp-8
			   (setf *key-move* t)
			   (incf (second *position*) 0.2))
			  (:scancode-kp-2
			   (setf *key-move* t)
			   (incf (second *position*) -0.2))))
	  (:keyup       (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  (:scancode-escape
			   (push-event :quit))
			  (:scancode-lctrl
			   (setf *mouse-right* nil))
			  (:scancode-lalt
			   (setf *mouse-left* nil))))
	  
	  (:mousemotion (:x x :y y :xrel xrel :yrel yrel)
			(setf *key-move* nil)
			(let ((asp (/ *screen-width* *screen-height*))
			      (half-width  (/ *screen-width*  2))
			      (half-height (/ *screen-height* 2)))
			  (setf *mouse-x*
				(float
				 (+ (* (/ (- x half-width)
					  half-width *zoom*)
				       asp)
				    (first *position*)))
				*mouse-y*
				(float
				 (+ (/ (- y half-height)
				       half-height *zoom* -1)
				    (second *position*))))
			  (when *mouse-right*
			    (incf (first  *position*)
				  (/ xrel half-height *zoom* -1))
			    (incf (second *position*)
				  (/ yrel half-height *zoom*)))))
	  (:mousebuttondown (:button button)
			    (case button
			      (1 (press-mouse-left))
			      (3 (setf *mouse-right* t))))
	  (:mousebuttonup   (:button button)
			    (case button
			      (1 (setf *mouse-left* nil))
			      (3 (setf *mouse-right* nil))))
	  (:mousewheel      (:y y)
			    (incf *zoom* (* y 0.1)))
			     
	  (:idle        ()
			(idler window))
	  (:windowevent (:event event :data1 width :data2 height)
			(when (=
			       event
			       sdl2-ffi:+sdl-windowevent-size-changed+)
			  (setf *screen-width* width
				*screen-height* height)
			  (resize-viewport width height)))
	  (:quit        ()
			(sdl2-ttf:quit)
			(clean-text-hash)
			(clean-texture-hash)
			t))))))

(main)

    
