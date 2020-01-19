;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

;; (ql:quickload :lispbuilder-sdl)

;; (defun width-height ()
;;   (let ((width 200) (height 200))
;;     (sdl:with-init ()
;;       (sdl:window width height :title-caption "Width and Height, from Processing.org")
;;       (setf (sdl:frame-rate) 5)
;;       (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))

;;       (loop for i from 0 to height by 20
;; 	 do (progn (sdl:draw-box (sdl:rectangle :x 0 :y i :w 200 :h 10)
;; 				  :color (sdl:color))
;; 		   (sdl:draw-box (sdl:rectangle :x i :y 0 :w 10 :h 200)
;; 				  :color (sdl:color :r 255 :g 255 :b 255))))
;;       (sdl:update-display)
;;       (sdl:with-events ()
;; 	(:quit-event () t)
;; 	(:video-expose-event () (sdl:update-display))))))

(defstruct gen
  width
  height
  arr)

(defparameter dead-cell 0)
(defparameter alive-cell 1)

(defmacro arr-idx (arr x y width height)
  `(aref ,arr (+ (* ,width ,y) ,x)))

(defun check-cell (arr x y width height)
  (if (or (< x 0) (< y 0) (>= x width) (>= y height))
	  0
		(if (= (aref arr (+ (* width y) x)) alive-cell)
		;; (if (/= (aref arr (+ (* width y) x)) 0)
			1
			0)))

(defun count-neighbors (arr x y width height)
  (+ (check-cell arr (+ x 1) y width height)
	 (check-cell arr (- x 1) y width height)
	 (check-cell arr x (+ y 1) width height)
	 (check-cell arr x (- y 1) width height)))

(defun gen-val (arr x y width height)
  (let ((neighbors (count-neighbors arr x y width height))
  		(val (arr-idx arr x y width height)))
  	(if (and (/= val alive-cell) (= neighbors 3))
  		alive-cell
  		(if (and (= val alive-cell) (or (= neighbors 3) (= neighbors 2)))
  			alive-cell
  			dead-cell))))

(defun next-gen (arr width height)
  (let ((new-arr (make-array (* width height) :initial-element dead-cell)))
	(loop for y from 0 to (- height 1)
	   do (loop for x from 0 to (- width 1)
			 do (setf (arr-idx new-arr x y width height) (gen-val arr x y width height))))
			 ;; do (setf (aref new-arr (+ (* width y) x)) (count-neighbors arr x y width height))))
	new-arr))

(defun print-gen (generation width height)
  (print "GEN")
  (loop for y from 0 to (- height 1)
	 do (print (subseq generation (* y width) (* (+ y 1) width)))))

(defun graphics-init (generation width height)
  (loop for i from 0 to 100
	 do (progn
		  (setf generation (next-gen generation width height))
		  (print-gen generation width height))))

(defparameter width nil)
(defparameter height nil)
(defparameter generation nil)

(defun main ()
  (if (/= (length *posix-argv*) 3)
	  (print "usage: sbcl --script game-of-life.lisp width height")
	  (progn
		(setf width (parse-integer (cadr *posix-argv*) :junk-allowed t))
		(setf height (parse-integer (caddr *posix-argv*) :junk-allowed t))
		(if (not (and width height))
			(print "Invalid parameters")
			(progn
			  (setf generation (make-array (* width height) :initial-element dead-cell))

			  (setf (arr-idx generation 10 10 width height) 1)
			  (setf (arr-idx generation 11 11 width height) 1)
			  (setf (arr-idx generation 9 12 width height) 1)
			  (setf (arr-idx generation 10 12 width height) 1)
			  (setf (arr-idx generation 11 12 width height) 1)

			  (print-gen generation width height)

			  (graphics-init generation width height))))))

(main)

;; (setf (aref generation 4) 1)
;; (print generation)
;; (print (count-neighbors generation 5 0 width height))
