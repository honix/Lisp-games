(ql:quickload :sketch)

(in-package :sketch)

(defstruct tank
  color x (ax 0.0) angle (aangle 0.0) (power 6.5) (apower 0.0))

(defstruct particle
  x y ax ay color (boom nil))

(defparameter *res* (list 500 200)) ; game size
(defparameter *mul* 2) ; pixel size
(defparameter *grass-color* (rgb 0.05 0.4 0.1))
(defparameter *grass-pen* (make-pen :fill *grass-color*
				    :weight 0))
(defparameter *terrain* (make-array (first *res*)
				    :initial-element 50
				    :element-type 'fixnum))
(defparameter *particles* ())
(defparameter *red-tank*  (make-tank :color +red+))
(defparameter *blue-tank* (make-tank :color +blue+))
(defparameter *winner* nil)

;; turn-controller
(let ((turn *red-tank*))
  (defun turn () turn)
  (defun make-turn () (setf turn (if (eq turn *red-tank*)
				     *blue-tank*
				     *red-tank*))))
;; utils
(defun random-center (x)
  (- (random x) (/ x 2)))

(defun clamp (min value max)
  (max min (min max value)))

(defun m (value)
  (* value *mul*))

(defun round-n-clamp (x)
  (clamp 5 (round x) (- (first *res*) 6)))

;; terrain
(defun new-map ()
  (setf *particles* ()
	(tank-x *red-tank*) 70
	(tank-angle *red-tank*) -4.14
	(tank-power *red-tank*) 6.5
	(tank-x *blue-tank*) (- (first *res*) 70)
	(tank-angle *blue-tank*) -2.14
	(tank-power *blue-tank*) 6.5
	*winner* nil)
  (let ((meter (+ (random-center 50) 100)))
    (dotimes (i (length *terrain*))
      (when (mod i 5)
	(incf meter (random-center 6.0)))
      (incf meter (sin meter))
      (setf (aref *terrain* i) (round meter)))))

(new-map)

(defun draw-terrain ()
  (with-pen *grass-pen*
    (dotimes (x (length *terrain*))
      (rect (m x) (m (aref *terrain* x))
	    *mul* 1000))))

(let* ((n 6)
       (range (loop for i from (- n) to n collect i)))
  (defun boom-terrain (x ax)
    (when (< 2 x (- (first *res*) 2))
      ;; make hole
      (let ((map (mapcar (lambda (p) (- (round x) p))
			 range))
	    (digger (mapcar (lambda (x) (- 6 (abs x)))
			    range)))
	(mapc (lambda (m d) (incf (aref *terrain* m) d)) map digger))
      ;; make dirt rain
      (dotimes (i 24)
	(push (make-particle :x x :y (- (pos-at x) 3)
			     :ax (+ (random-center 4.0)
				    (/ ax 2))
			     :ay (- (random-center 2.0) 2)
			     :color (rgb 0.3 0.2 0.0))
	      *particles*)))))

;; tank
(let* ((n 5)
       (range (loop for i from (- n) to n collect i))
       (len (length range)))
  (defun pos-at (x) 
    (let ((x (round-n-clamp x)))
      (let ((map (mapcar (lambda (z) (aref *terrain* z))
			 (mapcar (lambda (p) (+ x p))
				 range))))
	(values (/ (apply '+ map) len)
		(/ (apply #'+ (mapcar #'* map range))
		   n))))))

(defun update-tank (tank)
  (with-slots (x ax angle aangle power apower) tank
    (incf x ax)
    (incf angle aangle)
    (incf power apower)))

(defmacro with-translate (pos &rest body)
  (let ((x (first pos)) (y (second pos)))
  `(progn
     (translate ,x ,y)
     ,@body
     (translate (- ,x) (- ,y)))))

(defun draw-tank (tank)
  (with-slots (x angle) tank
    (multiple-value-bind (y ground-angle) (pos-at x)
      (with-pen (make-pen :fill (tank-color tank) :weight 0)
	(with-translate ((m x) (m (1- y)))
	  (with-pen (make-pen :stroke +black+ :weight 5)
	    (line 0 -6 (* (sin angle) 30) (* (cos angle) 30)))
	  (rotate ground-angle)
	  (rect -20 -5 40 10)
	  (rect -15 -10 30 10)
	  (rotate (- ground-angle)))))))

;; particles
(defun update-particles (tanks)
  (dolist (particle *particles*)
    (with-slots (x y ax ay color boom) particle
      (incf x ax)
      (incf y ay)
      (let ((mul 0.983))
	(setf ax (* ax mul))
	(setf ay (* ay mul)))
      (incf ay 0.1)
      (let ((pos (pos-at x)))
	;; pos is terrain y coordinate
	(when (<= pos y)
	  (if boom
	      ;; make hole
	      (progn
		(boom-terrain x ax)
		;; tank-hit
		(dolist (tank tanks)
		  (when (<= (- x 10) (tank-x tank) (+ x 10))
		    (setf *winner* x))))
	      ;; else make hill
	      (decf (aref *terrain* (round-n-clamp x))))
	  (setf *particles* (delete particle *particles*)))))))

(defun draw-particles ()
  (dolist (particle *particles*)
    (with-slots (x y color) particle
      (with-pen (make-pen :fill color)
	;; shift to center
	(labels ((s (x) (- (m x) (/ *mul* 2))))
	  (rect (s x) (s y) *mul* *mul*))))))

(defsketch tanks ((width  (* (first *res*)  *mul*))
		  (height (* (second *res*) *mul*))
		  (time 0.0))
  (incf time 0.1)
  (let* ((tanks (list *red-tank* *blue-tank*))
	 (power (* (tank-power (turn)) 3))
	 (tx (tank-x (turn)))
	 (x (m tx))
	 (y (- (m (pos-at tx)) 50)))
    (update-particles tanks)
    (unless *winner*
      (mapc #'update-tank tanks))
    (background (rgb 0.0 0.5 0.8))
    (draw-terrain)
    (draw-particles)
    (mapc #'draw-tank tanks)
    (with-pen (make-pen :fill (hsb 0 (/ power 40) 1))
      (rect (- x (/ power 2)) (- y 15) power 10))
    (ngon 3 x y 7 7 90))
  (text
   (if *winner*
       "game over!"
       "move A and D | altitude Q and E | magnitude W and S | shoot SPACE")
   240 (- (m (second *res*)) 30))
  (when *winner*
    (let ((size (* (sin time) 20)))
      (with-pen (make-pen :fill (rgb 0.9 0.4 0.2 0.8))
	(ngon 8 (m *winner*) (m (pos-at *winner*)) size size time)))))

(defmethod sdl2.kit:keyboard-event ((window tanks) state
				    ts repeat-p keysym)
  (with-slots (x ax angle aangle power apower) (turn) 
    (case state
      (:keydown
       (case (sdl2:scancode keysym)
	 (:scancode-a (setf ax -0.2))
	 (:scancode-d (setf ax 0.2))
	 (:scancode-e (setf aangle -0.01))
	 (:scancode-q (setf aangle 0.01))
	 (:scancode-w (setf apower 0.1))
	 (:scancode-s (setf apower -0.1))
	 (:scancode-r (new-map))))
      (:keyup
       (case (sdl2:scancode keysym)
	 ((:scancode-a :scancode-d) (setf ax 0.0))
	 ((:scancode-e :scancode-q) (setf aangle 0.0))
	 ((:scancode-w :scancode-s) (setf apower 0.0))
	 (:scancode-space
	  (if *winner* (new-map)
	      (let ((s (sin angle))
		    (c (cos angle)))
		(push (make-particle
		       :x (+ x (* s 16))
		       :y (+ (pos-at x) (* c 16))
		       :ax (* s power)
		       :ay (* c power)
		       :color +black+
		       :boom t)
		      *particles*)))
	  (setf ax 0.0)
	  (setf aangle 0.0)
	  (make-turn)))))))
     
(make-instance 'tanks)


