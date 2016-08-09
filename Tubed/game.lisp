;;
;; Game
;;

;(declaim (optimize (speed 3) (space 0) (safety 0)))

(ql:quickload '(:cl-opengl :sdl2-image :sdl2-ttf :sdl2-mixer))

(defpackage :tubed-game
  (:use :cl :cl-opengl :sdl2)
  (:export :main :save))

(in-package :tubed-game)

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
(defparameter *scene* :menu)
(defparameter *music* nil)
(defparameter *coin-sound* nil)
(defparameter *crash-sound* nil)

;; inputs
(defparameter *controls* 'controls) ; contains inputs

;; game
(defconstant +count+ 200) ; tube segments
(defconstant +car-position+ 15)
(defconstant +end-position+ 500)

(defparameter *speed* 0.0)
(defparameter *accel* 0.0)
(defparameter *position* 0.0)
(defparameter *next-position* 10.0)  
(defparameter *turn* 0.0)
(defparameter *rotate* 0.0)
(defparameter *tubed-list* #())
(defparameter *coins* 0)
(defparameter *segment* -1)

(defun reset ()
  (setf *time* -6.0
	*speed* 0.0
	*accel* 0.0
	*position* 0.0
	*next-position* 10.0
	*turn* 0.0
	*rotate* 0.0
	*tubed-list* #()
	*coins* 0
	*segment* -1))

(defun level-blocks (variant)
  (let ((list (case variant
		(:finish (loop for i to 15 collect
			      (make-tubed
			       :type :finish
			       :angle (/ i 2.5))))
		(:drop (list
			(make-tubed
			 :type :coin
			 :angle (* (/ (- *rotate* 180) 360) +2pi+)
			 :position 0.9
			 :speed (- (random 0.01))
			 :rotate (- (random 0.1) 0.05))))
		((0 4 6 8 15) (loop for i to 5 collect
			(make-tubed
			 :type :coin
			 :angle (+ +pi+ variant)
			 :position (- (/ i 10) 0.5))))
		((2 10 11) (loop for i to 5 collect
			(make-tubed
			 :type :coin
			 :angle (+ (/ i -7.0 (if (evenp variant) -1 1)) variant)
			 :position (- (/ i 10) 0.5))))
		((3 12 14 17 19) (list
		    (make-tubed
		     :type :mover
		     :angle (float variant)
		     :rotate -0.01)))
		(t (loop for i to 5 collect
				(make-tubed
				 :type :block
				 :angle (+ (/ i 3.5) variant)))))))
    (make-array (length list)
		:initial-contents list)))

(defun type-texture (type)
  (case type
    (:block "block.png")
    (:mover "mover.png")
    (:coin "brackets.png")
    (:finish "finish.png")
    (t "block.png")))


(defun punch-anglep (angle)
  "Check if object same angle with car"
  (let ((t-angle (mod (* (/ (- angle +pi+) +2pi+) 360)
		      360)))
    (when (> (+ t-angle 27) *rotate* (- t-angle 27))
      (- *rotate* t-angle))))

(defun collision (tubed)
  "Check collision and do something with it"
  (with-slots (type angle position) tubed
    (let ((car-pos (- 1 (/ +car-position+ +count+))))
      (when (and (> (+ car-pos 0.05) position car-pos)
		 (punch-anglep angle))
	(case type
	  ((:block :mover)
	      (setf *speed* -0.05)
	    (sdl2-mixer:play-channel 1 *crash-sound* 0)
	    (when (plusp *coins*)
	      (decf *coins* 1) :lost))
	  (:coin
	   (incf *coins* 1)
	   (sdl2-mixer:play-channel 1 *coin-sound* 0)
	   :delete)
	  (:finish
	   (setf *scene* :score)
	   (take-scores)))))))
			


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
					  (level-blocks :drop)
					  tube))))))
  
  ;; add new blocks
  (if (and (> *position* *next-position*)
	   (eq *scene* :game))
      (progn
	(setf *next-position* (+ *position* 20))
	(if (> *position* 400)
	    (concatenate 'vector
			 (level-blocks :finish)
			 tube)
	    (concatenate 'vector
			 (level-blocks (incf *segment*))
			 tube)))
      tube))

(defun trail (f position)
  "Returns organic shape for tube"
  (let ((position (/ position 20)))
    (funcall f (* (cos position) (sin (/ position 2)) 4))))

(defun game-screen (delta)
  "Update and render for game"
  (incf *time* delta)
  ;(gl:clear :color-buffer-bit)

  ;; controls
  (when (and (> *time* 0) (eq *scene* :game))
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
      (setf *accel* (- old-speed *speed*))))
  
  (setf *speed* (* *speed* 0.995))
  (incf *position* *speed*)
   
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
	 (let ((tunel (max 0.3 (sin (+ (/ i 10) *position*))))
	       (rev (/ (- count i) count))
	       (zoom (- (/ i 10))))
	   (gl:rotate (/ rev 2) tx ty 0)   ; make tube turns
	   (gl:color tunel tunel tunel)      ; colorize
	   ;; check tubed objects to draw
	   (loop for tubed across *tubed-list* do
		(with-slots (type angle position) tubed
		  (when (>= (+ rev (/ step count)) position rev)
		    (case type
		      ((:coin :finish)
		       (gl:color 1 1 1)
		       (gl:blend-func :src-alpha :one)))
		    (quad (load-texture (type-texture type))
			  (* (sin angle) 2.1)
			  (* (cos angle) 2.1)
			  zoom (* (/ angle +2pi+) -360) 6))))
	   ;; tube segment
	   (gl:blend-func :src-alpha :one-minus-src-alpha)
	   (quad (load-texture (if (> tunel 0.99)
				   (progn
				     (if (get *controls* 'down)
					 (gl:color 1 0 0)
					 (gl:color 1 1 1))
				     "white-gate.png")
				   "gate.png"))
		 0 0 zoom 0  30)))

    ;; draw the car
    (gl:load-identity)
    (when (minusp *time*)
      (gl:translate 0 0 (max 0 (- -3 *time*))))
    (let ((tunel (max 0.3 (sin (+ (/ +car-position+ 10) *position*)))))
      (gl:color tunel tunel tunel))    ; ambience for car
    (let* (; restore angles after rotation
	   (rotated (sin (* (/ *rotate* 360) +2pi+)))
	   ; car turning illusion
	   (shift (+ (* tx rotated -0.05) (* *turn* 0.025))))
      (gl:rotate (+ (random 1.5)
		    (* (random 10) *speed*)) 0 1 1) ; hi-speed shake
      (gl:translate (* *turn* 0.2) 0 0)
      (gl:rotate (* (+ *turn* tx) 5) 0 1 -1)
      (quad (load-texture "car-front.png") (- shift)
	    (+ -1.3 (* ty rotated 0.1)) -1.5 180 5)
      (quad (load-texture "car-middle.png") 0.0
	    (+ -1.3 (* ty rotated 0.05)) -1.5 180 5)
      (quad (load-texture "car-back.png") shift
	    -1.4 -1.3 180 5)))

  ;; gui
  (when (eq *scene* :game)
    (gl:load-identity)
    (when (<= -3 *time* 0)
      (gl:blend-func :src-alpha :one)
      (gl:color 1 1 1)
      (text (make-text (write-to-string (abs (floor *time*))))
	    0 0 (* (sin (mod *time* 1)) 1.5) 0))
    (when (> +pi+ *time* 0)
      (gl:blend-func :src-alpha :one)
      (gl:color 1 0.5 0.2 (sin *time*))
      (text (make-text "GO!")
	    0 0 (* (mod *time* +pi+) 1) 0))
    
    (gl:color 1 1 1 (min 1 *time*))

    (text (make-text "parenthesis")
	  -0.50 0.85 0.5 0)
    (text (make-text (write-to-string (floor *coins*)))
	  0.0 0.80 1.0 (* (sin (+ *time* *coins*)) 12))
    (text (make-text "dist")
	  0.80 -0.70 0.5 0)
    (text (make-text (write-to-string (- +end-position+ -30
					 (floor *position*))))
	  1.1 -0.70 0.5 (* (cos *time*) 12))
    (text (make-text "time")
	  0.80 -0.85 0.5 0)
    (text (make-text (write-to-string (floor *time*)))
	  1.1 -0.85 0.5 (* (sin *time*) 12))))


(defun menu-screen (delta)
  "Update and render for menu"
  (incf *time* delta)
  (gl:clear :color-buffer-bit)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (gl:color 1 1 1 0.05)
  (loop for i to 360 by 25 do
       (quad (load-texture "brackets.png")
	     0 0 0 (+ *time* i) 30))

  (gl:color 1 1 1 0.4)
  (loop for i to +2pi+ by 2 do
       (text (make-text "TUBED")
	     (cos (+ i *time*))
	     0.6
	     2 0))

  (gl:color 1 0.5 0.5)
  (text (make-text "august-2016-lisp-game-jam")
	0 0.4 0.3 0)
  (text (make-text "mission: collect parenthesis and avoid blocks")
	0 0.25 0.4 0)
  (gl:color 1 1 1)
  (text (make-text "press SPACE to ride")
	0 -0.15 0.7 0)
  (gl:color 1 0.5 0.5)
  (text (make-text "controll - arrow keys")
	0 -0.6 0.4 0)
  (text (make-text "go fullscreen - f11")
	0 -0.7 0.4 0)
  (text (make-text "exit (pause) - esc")
	0 -0.8 0.4 0))

(let ((best 0)
      (now 0))
  (defun score-screen (delta)
    (gl:load-identity)
    (gl:color 1 1 1)
    (text (make-text "~ YOUR LECENSE ~")
	  0 0.6 0.8 0)
    (text (make-text "!ranks based on parenthesis count and time!")
	  0 0.35 0.3 0)
    (text (make-text "Your score:")
	  -0.3 0.1 0.5 0)
    (text (make-text (write-to-string (floor now)))
	  0.3 0.1 1 0)
    (text (make-text "Your best:")
	  -0.3 -0.1 0.4 0)
    (text (make-text (write-to-string (floor best)))
	  0.3 -0.1 0.7 0)
    (text (make-text "RANK:")
	  -0.3 -0.45 0.7 0)
    (text (make-text (cond ((<= now 10)    "'F'")
			   ((<= 10 now 20) "'D'")
			   ((<= 20 now 30) "'C'")
			   ((<= 30 now 40) "'B'")
			   ((<= 40 now)    "'A'")))
	  0.3 -0.45 1 -10)
    (text (make-text "press SPACE")
	  0 -0.8 0.5 0))
  (defun take-scores ()
    (setf now (/ *coins* (/ *time* 20)))
    (setf best (max best now))))

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
		      (game-screen *delta*))
		     (:score
		      (game-screen *delta*)
		      (score-screen *delta*)))))))
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
    (sdl2-mixer:allocate-channels 2)
    (sdl2-mixer:volume 0 128) ; music
    (sdl2-mixer:volume 1 64) ; sound-effects
    (with-window (window :title "Tubed"
			 :w *screen-width*
			 :h *screen-height*
			 :flags '(:shown :resizable :opengl))
      (with-gl-context (gl-context window)
	(gl-make-current window gl-context)	
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(resize-viewport *screen-width* *screen-height*)
        
	(setf *music* (sdl2-mixer:load-music
		       (path "music.ogg"))
	      *coin-sound* (sdl2-mixer:load-wav
			    (path "coin.ogg"))
	      *crash-sound* (sdl2-mixer:load-wav
			     (path "crash.ogg")))

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
			   (case *scene*
			     (:menu
			      (setf *scene* :game)
			      (reset))
			     (:score
			      (setf *scene* :menu))))))
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
			     ((:game :score)
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

    
