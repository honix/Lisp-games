(ql:quickload :sketch)

(in-package :sketch)

(defparameter mouse-x 0)
(defparameter mouse-y 0)

(defun draw-tree (from power angle split)
  (if (> power 0)
      (destructuring-bind (x y) from
	(dotimes (brunch split)
	  (let* ((wide (/ split 4))
		 (current (- brunch wide))
		 (nx (+ x (* (cos angle) power)))
		 (ny (+ y (* (sin angle) power))))
	    (line x y nx ny)
	    (draw-tree `(,nx ,ny)
		       (- power 10)
		       (+ angle (* current mouse-y) mouse-x)
		       2))))))

(defsketch tree ()
  (draw-tree '(200 350) 70 (* +pi+ 1.5) 2))

(defmethod sdl2.kit:mousemotion-event ((window tree) ts bm
				       x y xrel yrel)
  (setf mouse-x (/ (- x 200) 200))
  (setf mouse-y (/ y 100)))

(make-instance 'tree)
