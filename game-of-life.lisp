;;; game-of-life.lisp --- Game of life using lispbuilder-sdl

; (setq *load-verbose* nil)

(ql:quickload 'lispbuilder-sdl)
;; (ql:quickload 'lispbuilder-sdl-gfx)
(ql:quickload 'unix-opts)

(defparameter *width* 200)
(defparameter *height* 200)

(defparameter *config-view-delta* 2)
(defparameter *config-timestep* 1.0)
(defparameter *config-timestep-delta* 2)

(defparameter *config-window-x* 512)
(defparameter *config-window-y* 512)

(defun update-view-size ()
  (setf *config-view-x* (round (/ *config-window-x* *config-view-size*)))
  (setf *config-view-y* (round (/ *config-window-y* *config-view-size*))))

(defparameter *config-view-size* 16)
(defvar *config-view-x*)
(defvar *config-view-y*)
(update-view-size)

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
		  1
		  0)))

(defun count-neighbors (arr x y width height)
  (+ (check-cell arr (+ x 1) y width height)
	 (check-cell arr (- x 1) y width height)
	 (check-cell arr x (+ y 1) width height)
	 (check-cell arr x (- y 1) width height)
	 (check-cell arr (+ x 1) (+ y 1) width height)
	 (check-cell arr (- x 1) (+ y 1) width height)
	 (check-cell arr (+ x 1) (- y 1) width height)
	 (check-cell arr (- x 1) (- y 1) width height)))

(defun gen-val (arr x y width height)
  (let ((neighbors (count-neighbors arr x y width height))
		(val (arr-idx arr x y width height)))
	(case val
	  (0 (if (= neighbors 3)
			 alive-cell
			 dead-cell))
	  (1 (case neighbors
		   (2 alive-cell)
		   (3 alive-cell)
		   (otherwise dead-cell)))
	  (otherwise -1))))
;; (if (and (/= val alive-cell) (= neighbors 3))
;; 	alive-cell
;; 	(if (and (= val alive-cell) (or (= neighbors 3) (= neighbors 2)))
;; 		alive-cell
;; 		dead-cell))))

(defun next-gen (arr width height)
  (let ((new-arr (make-array (* width height) :initial-element dead-cell)))
	(loop for y from 0 to (- height 1)
	   do (loop for x from 0 to (- width 1)
			 do (setf (arr-idx new-arr x y width height) (gen-val arr x y width height))))
	new-arr))

(defun print-gen (generation)
  (print "GEN")
  (loop for y from 0 to (- (gen-height generation) 1)
	 do (print (subseq (gen-arr generation)
					   (* y (gen-width generation))
					   (* (+ y 1) (gen-width generation))))))

(defun graphics-init (generation)
  (loop for i from 0 to 100
	 do (progn
		  (setf (gen-arr generation)
				(next-gen (gen-arr generation)
						  (gen-width generation)
						  (gen-height generation)))
		  (print-gen generation))))

(defparameter width nil)
(defparameter height nil)
(defparameter generation nil)

(defclass game ()
  ((paused :accessor game-paused :initform t)
   ;; current gen of life
   (state :accessor game-state :initform nil)
   ;; current view offset
   (vx :accessor game-vx :initform 0)
   (vy :accessor game-vy :initform 0)
   ;; SDL window
   (window :accessor game-window)))

(defmethod game-toggle-paused ((g game))
  (setf (game-paused g) (not (game-paused g))))

(defmethod game-step ((g game))
  (format "TODO~%"))

(defmethod game-reset ((g game))
  (format "TODO~%"))

(defmethod game-run ((g game))
  "Game-Of-Life docstring."
  (sdl:with-init ()
    ;; set up key repeat so holding down a key works
    (sdl:enable-key-repeat 200 30)
    (sdl:window width height :title-caption "Common Lisp Life")
    (setf (sdl:frame-rate) 60)
    (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))

    (loop for i from 0 to height by 20
          do (progn (sdl:draw-box (sdl:rectangle :x 0 :y i :w width :h 10)
                                  :color (sdl:color))
                    (sdl:draw-box (sdl:rectangle :x i :y 0 :w 10 :h height)
                                  :color (sdl:color :r 255 :g 255 :b 255))))
    (sdl:update-display)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      ;; process keyboard input
      ;; make living cell with [MouseLeftClick]
      (:key-down-event (:key key)
       ;; close window with [RedX] or [ESC]
       (when (sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event))
       ;; move view area with [W A S D] or [MouseDrag]
       (when (or (sdl:key= key :sdl-key-a) (sdl:key= key :sdl-key-left))
         (incf (game-vx g) *config-view-delta*))
       (when (or (sdl:key= key :sdl-key-d) (sdl:key= key :sdl-key-right))
         (decf (game-vx g) *config-view-delta*))
       (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-up))
         (decf (game-vy g) *config-view-delta*))
       (when (or (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-down))
         (incf (game-vy g) *config-view-delta*))
       ;; zoom view area with [+ -] or [MouseWheel]
       (when (sdl:key= key :sdl-key-minus)
         (when (> *config-view-size* 2) ;; minimum of 2x2
           (decf *config-view-size* 1)
           (update-view-size)))
       (when (sdl:key= key :sdl-key-plus)
         (when (< *config-view-size* (+ *config-window-x* 1)) ;; TODO: square window only?
           (incf *config-view-size* 1)
           (update-view-size)))
       (when (sdl:key= key :sdl-key-equals)
         (setf *config-view-size* 16)
         (update-view-size))
       ;; reset game state and pause with [R]
       (when (sdl:key= key :sdl-key-r)
         (game-reset g)
         (setf (game-paused g) t))
       ;; toggle pause sim with [P]
       (when (sdl:key= key :sdl-key-p)
         (game-toggle-paused g))
       ;; adjust timestep with [< >] or [MouseWheel+SHIFT]
       (when (sdl:key= key :sdl-key-less)
         (when (< *config-timestep* most-positive-single-float)
           (setf *config-timestep* (* *config-timestep* *config-timestep-delta*))))
       (when (sdl:key= key :sdl-key-greater)
         (when (> *config-timestep* 0.0)
           (setf *config-timestep* (/ *config-timestep* *config-timestep-delta*))))
       ; TODO mouse input
       )
      ;; if no input
      (:idle ()
       ;; todo
       ;; update game at end
       (game-step g)
       (sdl:update-display))
      )))

(defun main ()
  (if (/= (length *posix-argv*) 3)
	  (print "usage: sbcl --script game-of-life.lisp width height")
	  (let ((width (parse-integer (cadr *posix-argv*) :junk-allowed t))
			(height (parse-integer (caddr *posix-argv*) :junk-allowed t)))
		(if (not (and width height))
			(print "Invalid parameters")
			(let ((generation (make-gen :width width :height height :arr (make-array (* width height) :initial-element dead-cell))))
			  (setf (arr-idx (gen-arr generation) 10 10 width height) 1)
			  (setf (arr-idx (gen-arr generation) 11 11 width height) 1)
			  (setf (arr-idx (gen-arr generation) 9 12 width height) 1)
			  (setf (arr-idx (gen-arr generation) 10 12 width height) 1)
			  (setf (arr-idx (gen-arr generation) 11 12 width height) 1)

			  ;; (setf (arr-idx (gen-arr generation) 10 10 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 11 10 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 10 11 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 11 11 width height) 1)

			  (print generation)

			  (print-gen generation)

			  (graphics-init generation))))))

(sb-int:with-float-traps-masked (:invalid :inexact :overflow)
                                (main))
;; (setf (aref generation 4) 1)
;; (print generation)
;; (print (count-neighbors generation 5 0 width height))
