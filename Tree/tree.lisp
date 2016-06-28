(ql:quickload :sketch)

(in-package :sketch)

(declaim (optimize (speed 3)))

(defparameter mouse-x 0)
(defparameter mouse-y 0)

(defun draw-tree (from power angle split)
  (declare (float angle) (fixnum power split))
  (if (> power 0)
      (destructuring-bind (x y) from
	(dotimes (brunch split)
	  (let* ((wide (/ split 4))
		 (current (- brunch wide))
		 (nx (+ x (* (cos angle) power)))
		 (ny (+ y (* (sin angle) power))))
	    (with-pen (make-pen :weight (/ power 7) :stroke +black+)
	      (line x y nx ny))
	    (draw-tree `(,nx ,ny)
		       (- power 7)
		       (+ angle (* current mouse-y) mouse-x)
		       2))))))

(defsketch tree ((width 600) (height 600))
  (background (rgb 0.3 0.5 0.6))
  (with-pen (make-pen :fill (rgb 0.99 0.9 0.4))
    (circle 200 300 300))
  (with-pen (make-pen :fill (gray 0.1))
    (rect 0 500 600 200))
  (draw-tree '(300 500) 70 (* +pi+ 1.5) 2))


(defmethod sdl2.kit:mousemotion-event ((window tree) ts bm
				       x y xrel yrel)
  (setf mouse-x (/ (- x 300) 300)
	mouse-y (/ y 200)))

(make-instance 'tree)
