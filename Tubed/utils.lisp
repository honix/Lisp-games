(in-package :tubed-game)

(defun path (file-name)
  (merge-pathnames (concatenate 'string "media/" file-name)))

;;;
;;; loading-utils
;;;

(defstruct text-texture
  texture width heigth)

(defparameter *text-hash* (make-hash-table :test #'equal))

(defun clean-text-hash ()
  (maphash (lambda (key value) 
	     (gl:delete-textures (list (text-texture-texture value))))
	   *text-hash*)
  (clrhash *text-hash*))

(defun make-text (string)
  (or (gethash string *text-hash*)
      (progn
	(when (> (hash-table-count *text-hash*) 64)
	  (clean-text-hash))
       (let* ((font         (sdl2-ttf:open-font
			     (path "saxmono.ttf") 30))
	      (texture      (car (gl:gen-textures 1)))
	      (surface      (sdl2-ttf:render-utf8-blended
			     font string 255 255 255 0))
	      (surface-w    (surface-width surface))
	      (surface-h    (surface-height surface))
	      (surface-data (surface-pixels surface)))
	 (gl:bind-texture  :texture-2d texture)
	 (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	 (gl:tex-image-2d  :texture-2d 0 :rgba surface-w surface-h 0
			   :rgba :unsigned-byte surface-data)
	 (free-surface surface)
	 (sdl2-ttf:close-font font)
	 (setf (gethash string *text-hash*)
	       (make-text-texture :texture texture
				  :width (/ surface-w 30)
				  :heigth (/ surface-h 30 -1)))))))


(defparameter *texture-hash* (make-hash-table :test #'equal))

(defun clean-texture-hash ()
  (maphash (lambda (key value) 
	     (gl:delete-textures (list value)))
	   *texture-hash*)
  (clrhash *texture-hash*))

(defun load-texture (file-name &optional (format :rgba)
				         (filter :linear))
  (or (gethash file-name *texture-hash*)
      (progn
	(when (> (hash-table-count *texture-hash*) 64)
	  (clean-texture-hash))
       (let* ((texture      (car (gl:gen-textures 1)))
	      (surface      (sdl2-image:load-image (path file-name)))
	      (surface-w    (surface-width surface))
	      (surface-h    (surface-height surface))
	      (surface-data (surface-pixels surface)))
	 (gl:bind-texture  :texture-2d texture)
	 (gl:tex-parameter :texture-2d :texture-min-filter filter)
	 (gl:tex-parameter :texture-2d :texture-mag-filter filter)
	 (gl:tex-image-2d  :texture-2d 0 :rgba surface-w surface-h 0
			   format ; :bgr - bmp  :rgba - png
			   :unsigned-byte surface-data)
	 (free-surface surface)
	 (setf (gethash file-name *texture-hash*) texture)))))

;;;
;;; drawing
;;;

(defmacro defshape (fname &body body)
  `(defun ,fname (texture x y z rotation
		  scale-x &optional scale-y)
     (gl:bind-texture :texture-2d texture)
     (gl:push-matrix)
     (gl:translate x y z)
     (gl:rotate rotation 0 0 1)
     (gl:scale scale-x (or scale-y scale-x) 1)
     ,@body
     (gl:end)
     (gl:pop-matrix)))

(defshape quad
  (gl:begin :triangle-fan)
  (gl:tex-coord 0.0 0.0) (gl:vertex -0.1 -0.1)
  (gl:tex-coord 1.0 0.0) (gl:vertex  0.1 -0.1)
  (gl:tex-coord 1.0 1.0) (gl:vertex  0.1  0.1)
  (gl:tex-coord 0.0 1.0) (gl:vertex -0.1  0.1))

#|
(defshape triangle
  (gl:begin :triangles)
  (gl:tex-coord 0.5 0.0) (gl:vertex  0.0  0.1)
  (gl:tex-coord 1.0 1.0) (gl:vertex -0.1 -0.1)
  (gl:tex-coord 0.0 1.0) (gl:vertex  0.1 -0.1))
|#

(defun text (text-texture x y size rotation)
  (with-slots (texture width heigth) text-texture
    (quad texture x y 0.0
	  rotation (* width size) (* heigth size))))

;;;
;;; window-utils
;;;

(defun resize-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((asp (/ width height)))
    (gl:frustum (- asp) asp -1 1 1 100))
  (gl:translate 0 0 -1)
  (gl:matrix-mode :modelview)
  (gl:clear-color 0.1 0.0 0.0 1))

(defun full-screen (window)
  (multiple-value-bind (some width height)
      (get-current-display-mode 0)
    (set-window-size window width height))
  (set-window-fullscreen
   window
   (setf *fullscreen*
	 (not *fullscreen*)))
  (if *fullscreen* (hide-cursor) (show-cursor)))

;;;
;;; exebutable
;;;

(defun save ()
  "Make exebutable"
  #+sbcl(if (string= (software-type) "Linux")
	    (sb-ext:save-lisp-and-die
	     (concatenate 'string
			  "_builds/abra-" (software-type))
	     :toplevel #'main :executable t :compression 9)
	    (sb-ext:save-lisp-and-die
	     (concatenate 'string
			  "_builds/abra-" (software-type) ".exe")
	     :toplevel #'main :executable t :application-type :gui))
  #+ccl(ccl:save-application
	(concatenate 'string "_builds/abra-ccl-" (software-type))
	:toplevel-function #'main
	:prepend-kernel t))
