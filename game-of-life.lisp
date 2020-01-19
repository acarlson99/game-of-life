(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :lispbuilder-sdl)

(defun width-height ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Width and Height, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))

      (loop for i from 0 to height by 20
	 do (progn (sdl:draw-box (sdl:rectangle :x 0 :y i :w 200 :h 10)
				  :color (sdl:color))
		   (sdl:draw-box (sdl:rectangle :x i :y 0 :w 10 :h 200)
				  :color (sdl:color :r 255 :g 255 :b 255))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))

(defun check-cell (arr x y width height)
  (if (or (< x 0) (< y 0) (>= x width) (>= y height))
	  0
	(progn
	(print (aref arr (+ (* width y) x)))
	(print x)
	(print y)
	(if (= (aref arr (+ (* width y) x)) 1)
		1
	  0)))
  )

(defun count-neighbors (arr x y width height)
  (+ (check-cell arr (+ x 1) y width height)
     (check-cell arr (- x 1) y width height)
     (check-cell arr x (+ y 1) width height)
     (check-cell arr x (- y 1) width height)))

;; (defun next-gen (arr width height)
;;   (setq new-arr (* width height) :initial-element 0)
;;   (loop for y from 0 to height
;; 		(loop for x from 0 to width
;; 			  (setf (aref new-arr (+ (* width y) x)) (count-neighbors arr x y width height))))
;;   new-arr)

(defparameter width 20)
(defparameter height 20)
(defparameter generation (make-array (* width height) :initial-element 0))

(defun main ()
  (print "ABC"))

(main)

(setf (aref generation 4) 1)
(print generation)
(print (count-neighbors generation 5 0 width height))
