(ql:quickload :sketch)

(in-package :sketch)

(defparameter *time* 0.0)
(defparameter *turn* :circle)
(defparameter *winner* nil)
(defparameter *cells* nil)

(defparameter *table*
  (let ((lst))
    (dotimes (i 3 (nreverse lst))
      (dotimes (j 3)
	(push (list i j) lst)))))

(defparameter *win-patterns*
  (list '(0 1 2) '(3 4 5) '(6 7 8)
	'(0 3 6) '(1 4 7) '(8 5 2)
	'(0 4 8) '(2 4 6)))

(defparameter *positions*  
  (let ((unocell (/ 400 4))
	(lst))
    (dolist (cell *table* (nreverse lst))
      (destructuring-bind (x y) cell
	(push (list (* unocell (+ 1 x))
		    (* unocell (+ 1 y)))
	      lst)))))

(defun restart-game ()
  (setf *winner* nil
	*cells* (loop for i from 1 to (* 3 3)
		   collect :empty)))

(restart-game)

;; utils

(defmacro with-identity (&rest body)
  "Do body and restore matrix"
  `(progn
     (push-matrix)
     ,@body
     (pop-matrix)))

(defun center-quad (pos &optional pen)
  "Draw a quad"
  (let* ((size 90)
	 (half (/ size 2)))
    (destructuring-bind (x y) pos
      (with-pen (or pen (make-pen :fill (rgb 0.9 0.9 0.5)))
	(rect (- x half) (- y half) size size)))))

(defun center-circle (pos)
  "Draw a circle"
  (destructuring-bind (x y) pos
    (with-pen (make-pen :fill nil :stroke +black+ :weight 10)
      (circle x y 20))))

(defun center-cross (pos)
  "Draw a cross"
  (destructuring-bind (x y) pos
    (with-pen (make-pen :fill +black+)
      (with-identity
	  (translate x y)
	(rotate 45)
	(rect -5 -40 10 80)
	(rotate 90)
	(rect -5 -40 10 80)))))

(defun turn (fig)
  "Switch turn"
  (case fig
    (:circle :cross)
    (:cross  :circle)))

(defun make-turn ()
  "Switch *turn* and return previous"
  (let ((mem *turn*))
    (setf *turn* (turn *turn*))
    mem))

(defun positions (item list)
  "Returns list of indexes"
  (let ((poses))
    (dotimes (i (length list) (nreverse poses))
      (when (eq (nth i list) item)
	(push i poses)))))

(defun choose (seq)
  "Ruturns random object from sequence"
  (nth (random (length seq)) seq))

(defun shuffle (seq)
  "Returns shuffled sequence"
  (let ((copy (copy-list seq))
	(new))
    (dotimes (i (length copy) new)
      (let ((obj (choose copy)))
	(push obj new)
	(setf copy (remove obj copy))))))

;; ai

(defun check-winner (cells &optional silent)
  "Return winner if silent else set new *winner*"
  (dolist (row *win-patterns*)
    (labels ((got (n) (nth n cells))
	     (check (key) (= (count key (mapcar #'got row))
			     3)))
      (cond
	((check :cross) (if silent
			    (return :cross)
			    (setf *winner* :cross)))
	((check :circle) (if silent
			     (return :circle)
			     (setf *winner* :circle)))
	(t nil)))))

(defun ai-turn (cells)
  "Return best turn for current cells"
  (let* ((emptys (positions :empty cells))
	 (best (car (shuffle emptys)))
	 (best-count 100))
    (labels ((ai-dig (cells turn &optional first (count 0) last)
	       (case (check-winner cells :silent)
		 (:circle (when (< count best-count)
			    (setf best first
				  best-count count)))
		 (:cross (when (= count 1)
			   (setf best last
				 best-count (1+ count))))
		 (t (dolist (cell (shuffle (positions :empty cells)))
		      (let ((copy (copy-list cells)))
			(setf (nth cell copy) turn)
			(ai-dig copy (turn turn) (or first cell)
				(1+ count) cell)))))))
      (if (= (length emptys) 9)
	  4 ; easy center turn
	  (progn
	    (ai-dig cells :cross)
	    (ai-dig cells :circle)
	    best)))))

(let ((timer 0.0)
      (wait-time 2.5))
  (defun wait ()
    (when (> (incf timer 0.1) wait-time)
      (setf timer 0.0))))

;; app

(defsketch tick ()
  (incf *time* 0.06)
  
  ;; computer-turn
  (when (and (not *winner*) (eq *turn* :circle)
	     (positions :empty *cells*) (wait))
    (setf (nth (ai-turn *cells*) *cells*) (make-turn))
    (check-winner *cells*))
  
  ;; bg render
  (with-identity
      (translate 200 200)
    (rotate *time*)
    (scale 3)
    (center-quad '(0 0) (make-pen :fill (rgb 0.2 0.7 0.6)))
    (case *turn*
      (:circle (center-circle '(0 0)))
      (:cross (center-cross '(0 0)))))

  ;; board and figures render
  (dotimes (i (* 3 3))
    (center-quad (nth i *positions*))
    (case (nth i *cells*)
      (:circle (center-circle (nth i *positions*)))
      (:cross (center-cross (nth i *positions*)))))

  ;; winner message
  (when *winner*
    (with-identity
	(translate (* (sin *time*) 50) 45)
      (text (case *winner*
	      (:circle "CIRCLE IS WINNER")
	      (:cross  "CROSS IS WINNER"))
	    50 -50 300 60))))


(defmethod sdl2.kit:mousebutton-event ((window tick)
				       state ts
				       button x y)
  (when (eq state :mousebuttondown)
    (cond
      (*winner* (restart-game))
      ((= (count :empty *cells*) 0) (restart-game))
      (t (dotimes (i (* 3 3))
	   (destructuring-bind (cx cy) (nth i *positions*)
	     (when (and (eq (nth i *cells*) :empty)
			(< (- cx 50) x (+ cx 50))
			(< (- cy 50) y (+ cy 50)))
	       (setf (nth i *cells*) (make-turn))
	       (check-winner *cells*)
	       (return))))))))

(defmethod sdl2.kit:keyboard-event ((window tick)
				    state tx
				    repeat-p keysym)
  (when (sdl2:scancode= keysym :scancode-r)
    (restart-game)))

(make-instance 'tick)
