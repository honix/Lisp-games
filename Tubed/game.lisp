;;
;; Game
;;

;(declaim (optimize (speed 3) (space 0) (safety 0)))

(ql:quickload '(:cl-opengl :sdl2-image :sdl2-ttf :sdl2-mixer))

(defpackage :abra
  (:use :cl :cl-opengl :sdl2)
  (:export :main :save))

(in-package :abra)

(load (merge-pathnames "utils.lisp"))

(defstruct tubed ; tube-object
  (type :none   :type keyword)
  (angle 0.0    :type short-float)
  (position 0.0 :type short-float)
  (speed 0.0    :type short-float)
  (rotate 0.0   :type short-float))

(defconstant +pi+ 3.14159)
(defconstant +2pi+ (* 2 +pi+))

;; main
(defparameter *screen-width* 700)
(defparameter *screen-height* 500)
(defparameter *fullscreen* nil)
(defparameter *delta* 0.0)
(defparameter *time* 0.0)
(defparameter *music* nil)
(defparameter *scene* :menu)

;; inputs
(defparameter *controls* 'controls) ; contains inputs

;; game
(defun reset ()
  (defconstant +count+ 200) ; tube segments
  (defparameter *speed* 0.0)
  (defparameter *accel* 0.0)
  (defparameter *position* 0.0)
  (defconstant +car-position+ 15)
  (defparameter *turn* 0.0)
  (defparameter *rotate* 0.0)
  (defparameter *tubed-list* #())
  (defparameter *coins* 0))

(reset)


(defun level-blocks (variant)
  (let* ((random-angle (random +2pi+))
	 (list (case variant
		 (-1 (list
		      (make-tubed
		       :type :coin
		       :angle (* (/ (- *rotate* 180) 360) +2pi+)
		       :position 0.9
		       :speed (- (random 0.01))
		       :rotate (- (random 0.1) 0.05))))
		 (0 (loop for i to 5 collect
			 (make-tubed
			  :type :block
			  :angle (+ (/ i 3.5) random-angle)
			  :position 0.0)))
		 (1 (loop for i to 5 collect
			 (make-tubed
			  :type :coin
			  :angle (+ (/ i 10) random-angle)
			  :position (- (/ i 10) 0.5)))))))
    (make-array (length list)
		:initial-contents list)))

(defun type-texture (type)
  (case type
    (:block "block.png")
    (:coin "brackets.png")))


(defun punch-anglep (angle)
  "Check if object same angle with car"
  (let ((t-angle (mod (* (/ (- angle +pi+)
			    +2pi+)
			 360)
		      360)))
    (when (> (+ t-angle 20) *rotate* (- t-angle 20))
      (- *rotate* t-angle))))

(defun collision (tubed)
  "Check collision and do something with it"
  (with-slots (type angle position) tubed
    (let ((car-pos (- 1 (/ +car-position+ +count+)))
	  (angle (punch-anglep angle)))
      (when (and (> (+ car-pos 0.02) position car-pos) angle)
	(case type
	  (:block
	    (setf *speed* -0.05)
	    (when (plusp *coins*)
	      (decf *coins* 1) :lost))
	  (:coin
	    (incf *coins* 1) :delete))))))
			

(let ((next-position 10))
  (defun update-tubed (tube)
    "Make new tubeds and increase positions"
    ;; delete out-of-game tubeds
    (let ((out-of-game (position-if
			(lambda (tubed)
			  (> (slot-value tubed 'position) 1.5))
			tube :from-end t)))
      (when out-of-game
	(setf tube (subseq tube 0 out-of-game))))
    
    ;; increase positions and check car-collision
    (let ((current-speed *speed*))
      (loop for tubed across tube do
	   (with-slots (position speed angle rotate) tubed
	     (incf position (+ (/ current-speed 20)
			       speed))
	     (incf angle rotate))
	   (case (collision tubed)
		 (:delete (setf tube (delete tubed tube)))
		 (:lost (setf tube (concatenate 'vector
						(level-blocks -1)
						tube))))))
    
    ;; add new blocks
    (if (> *position* next-position)
	(progn
	  (setf next-position (+ *position* 10))
	  (concatenate 'vector
		       (level-blocks (random 2))
		       tube))
	tube)))

(defun trail (f position)
  "Returns organic shape for tube"
  (let ((position (/ position 20)))
    (funcall f (* (cos position) (sin (/ position 2)) 4))))

(defun game-screen (delta)
  "Update and render for game"
  (incf *time* delta)
  (gl:clear :color-buffer-bit)

  ;; controls
  (let ((factor (min (* *speed* 4) 0.25)))
    (when (get *controls* 'left)  (incf *turn* factor))
    (when (get *controls* 'right) (decf *turn* factor)))
  (setf *turn* (* *turn* 0.90))
  (incf *rotate* (if (plusp *speed*) *turn* (- *turn*)))
  (setf *rotate* (mod *rotate* 360))

  (let ((old-speed *speed*))
    (when (get *controls* 'up)   (incf *speed* 0.002))
    (when (and (plusp *speed*) (get *controls* 'down))
      (decf *speed* 0.002))
    (setf *speed* (* *speed* 0.995))
    (incf *position* *speed*)
    (setf *accel* (- old-speed *speed*)))
   
  ;; update
  (setf *tubed-list* (update-tubed *tubed-list*))

  ;; render
  (let ((count +count+)
	(step 1)
	(tx (trail #'sin *position*))
	(ty (trail #'cos *position*))
	(fly (mod *position* 1)))
    (gl:scale 1 1 1.8)              ; fov
    (gl:rotate *rotate* 0 0 1)      ; rotate tube
    (gl:rotate -45 tx ty 0)         ; look forward
    ;; draw tube from far to close
    (loop for i from (+ count fly) downto fly by step do
	 (let ((tunel (sin (+ (/ i 10) *position*)))
	       (rev (/ (- count i) count))
	       (zoom (- (/ i 10))))
	   (gl:rotate (/ rev 2) tx ty 0)   ; make tube turns
	   (gl:color rev tunel tunel)      ; colorize
	   ;; check tubed objects to draw
	   (loop for tubed across *tubed-list* do
		(with-slots (type angle position) tubed
		  (when (>= (+ rev (/ step count)) position rev)
		    (when (eq type :coin)
		      (gl:color 1 1 1)
		      (gl:blend-func :src-alpha :one))
		    (quad (load-texture (type-texture type))
			  (* (sin angle) 2.1)
			  (* (cos angle) 2.1)
			  zoom (* (/ angle +2pi+) -360) 6.0))))
	   ;; tube segment
	   (gl:blend-func :src-alpha :one-minus-src-alpha)
	   (quad (load-texture (if (> tunel 0.99)
				   (progn
				     (gl:color (* *accel* 600)
					       0.5
					       1)
				     "white-gate.png")
				   "test4.png"))
		 0.0 0.0 zoom *position* 30.0)))

    ;; draw the car
    (gl:load-identity)
    (let ((tunel (sin (+ (/ +car-position+ 10) *position*)))
	  (rev (/ (- count +car-position+) count)))
      (gl:color rev tunel tunel))    ; ambience for car
    (let* (; restore angles after rotation
	   (rotated (sin (* (/ *rotate* 360) +2pi+)))
	   ; car turning illusion
	   (shift (+ (* tx rotated -0.05) (* *turn* 0.025))))
      (gl:rotate (+ (random 1.5)
		    (* (random 10) *speed*)) 0 1 1) ; hi-speed shake
      (gl:translate (* *turn* 0.2) 0 0)
      (gl:rotate (* (+ *turn* tx) 5) 0 1 -1)
      (quad (load-texture "car-front.png") (- shift)
	    (+ -1.3 (* ty rotated 0.1)) -1.5 180.0 5.0)
      (quad (load-texture "car-middle.png") 0.0
	    (+ -1.3 (* ty rotated 0.05)) -1.5 180.0 5.0)
      (quad (load-texture "car-back.png") shift
	    -1.4 -1.3 180.0 5.0)))

  ;;; gui
  (gl:load-identity)
  (gl:color 1 1 1)
  (text (make-text "parenthesis")
	-0.50 0.85 0.5 0.0)
  (text (make-text (write-to-string (floor *coins*)))
	0.0 0.80 1.0 (* (sin (+ *time* *coins*)) 12.0))
  (text (make-text "dist")
	0.80 -0.70 0.5 0.0)
  (text (make-text (write-to-string (floor *position*)))
	1.1 -0.70 0.5 (* (cos *time*) 12.0))
  (text (make-text "time")
	0.80 -0.85 0.5 0.0)
  (text (make-text (write-to-string (floor *time*)))
	1.1 -0.85 0.5 (* (sin *time*) 12.0)))


(defun menu-screen (delta)
  "Update and render for menu"
  (incf *time* delta)
  (gl:clear-color 0 0 0 0.1)
  (gl:clear :color-buffer-bit)

  (gl:color 1 1 1 0.05)
  (loop for i to 360 by 25 do
       (quad (load-texture "brackets.png")
	     0.0 0.0 0.0 (+ *time* i) 30.0))

  (gl:color 1 1 1 0.4)
  (loop for i to +2pi+ by 2 do
       (text (make-text "TUBED")
	     (cos (+ i *time*))
	     0.6
	     2.0 0.0))

  (gl:color 1.0 0.5 0.5)
  (text (make-text "mission: collect parenthesis and avoid blocks")
	0.0 0.3 0.4 0.0)
  (gl:color 1 1 1)
  (text (make-text "press SPACE to ride")
	0.0 -0.15 0.7 0.0)
  (gl:color 1 0.5 0.5)
  (text (make-text "controll - arrow keys")
	0.0 -0.6 0.4 0.0)
  (text (make-text "go fullscreen - f11")
	0.0 -0.7 0.4 0.0)
  (text (make-text "exit (pause) - esc")
	0.0 -0.8 0.4 0.0))


(defun idler (window)
  "Every frame routines"
  (let ((old-time (get-internal-real-time))
	(errors (nth-value
		 1
		 (ignore-errors ; in-game error messager
		   (case *scene*
		     (:menu
		      (menu-screen *delta*))
		     (:game
		      (game-screen *delta*)))))))
    (when errors
      (gl:load-identity)
      (gl:color 1 1 1 1)
      (text (make-text
	     (write-to-string errors))
	    0.0 0.0 0.4 0.0))
    
    (gl:flush)
    (gl-swap-window window)
    
    (setf *delta*
	  (/ (- (get-internal-real-time)
		old-time)
	     internal-time-units-per-second))))
  

(defun main()
  "Init all stuff and define events"
  (with-init (:everything)
    (sdl2-ttf:init)
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 1)
    (sdl2-mixer:volume 0 128)
    (with-window (window :title "abracadabra"
			 :w *screen-width*
			 :h *screen-height*
			 :flags '(:shown :resizable :opengl))
      (with-gl-context (gl-context window)
	(gl-make-current window gl-context)	
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(resize-viewport *screen-width* *screen-height*)
        
	(setf *music* (sdl2-mixer:load-music (path "music.ogg")))
	
	(sdl2-mixer:play-music *music*)

	(sdl2:with-event-loop (:method :poll)
	  (:keydown     (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  ((:scancode-up :scancode-kp-8)
			   (setf (get *controls* 'up) t))
			  ((:scancode-down :scancode-kp-5)
			   (setf (get *controls* 'down) t))
			  ((:scancode-left :scancode-kp-4)
			   (setf (get *controls* 'left) t))
			  ((:scancode-right :scancode-kp-6)
			   (setf (get *controls* 'right) t))
			  (:scancode-space
			   (setf *scene* :game))))
	  (:keyup       (:keysym keysym)
			(case (scancode-symbol
			       (scancode-value keysym))
			  ((:scancode-up :scancode-kp-8)
			   (setf (get *controls* 'up) nil))
			  ((:scancode-down :scancode-kp-5)
			   (setf (get *controls* 'down) nil))
			  ((:scancode-left :scancode-kp-4)
			   (setf (get *controls* 'left) nil))
			  ((:scancode-right :scancode-kp-6)
			   (setf (get *controls* 'right) nil))
			  (:scancode-escape
			   (case *scene*
			     (:menu
			      (push-event :quit))
			     (:game
			      (setf *scene* :menu))))
			  (:scancode-f11
			   (full-screen window))))
	  ; (:x x :y y :xrel xrel :yrel yrel :state state)
	  ;(:mousemotion (:x x :y y))
	  (:idle        ()
			(idler window))
	  (:windowevent (:event event :data1 width :data2 height)
			(when (=
			       event
			       sdl2-ffi:+sdl-windowevent-size-changed+)
			   (resize-viewport width height)))
	  (:quit        ()
			(sdl2-ttf:quit)
			(sdl2-mixer:free-music *music*)
			(sdl2-mixer:halt-music)
			(sdl2-mixer:close-audio)
			(sdl2-mixer:quit)
			(clean-text-hash)
			(clean-texture-hash)
			t))))))

(main)

    
