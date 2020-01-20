;;; game-of-life.lisp --- Game of life using lispbuilder-sdl

; (setq *load-verbose* nil)

(ql:quickload 'lispbuilder-sdl)
;; (ql:quickload 'lispbuilder-sdl-gfx)

(defparameter *width* 200)
(defparameter *height* 200)

(defparameter *config-view-delta* 1)
(defparameter *config-timestep* 1.0)
(defparameter *config-timestep-delta* 2)

(defparameter *config-window-x* 512)
(defparameter *config-window-y* 512)

(defparameter *cell-size* 32)

;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

;; (ql:quickload :lispbuilder-sdl)


(defclass game ()
  ((paused :accessor game-paused :initform t)
   ;; current gen of life
   (state :accessor game-state :initform nil)
   (next :accessor game-next :initform nil)
   ;; current view offset
   (vx :accessor game-vx :initform 0)
   (vy :accessor game-vy :initform 0)
   (width :accessor game-width :initform 32)
   (height :accessor game-height :initform 32)
   ;; SDL window
   (window :accessor game-window)))

(defstruct gen
  width
  height
  arr)

(defparameter dead-cell 0)
(defparameter alive-cell 1)

(defmacro arr-idx (arr x y width height)
  `(aref ,arr (+ (* ,width ,y) ,x)))

(defun arr-idx* (arr x y width height)
  (aref arr (+ (* width y) x)))

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

(defun next-gen (arr width height &optional clear)
  (let ((new-arr (make-array (* width height) :initial-element dead-cell)))
	(if (not clear)
		(loop for y from 0 to (- height 1)
		   do (loop for x from 0 to (- width 1)
				 do (setf (arr-idx new-arr x y width height) (gen-val arr x y width height)))))
	new-arr))

(defun print-gen (generation)
  (print "GEN")
  (loop for y from 0 to (- (gen-height generation) 1)
	 do (print (subseq (gen-arr generation)
					   (* y (gen-width generation))
					   (* (+ y 1) (gen-width generation))))))

(defmethod game-setup ((g game) width height)
  (sdl:with-init ()
    (setf (game-window g)
	  (sdl:window *config-window-x* *config-window-y*
	    :title-caption "beans lol"))
    ;; set up key repeat so holding down a key works
    ; (setf *width* width)
    ; (setf *height* height)
    (setf (game-width g) width)
    (setf (game-height g) height)
    (sdl:enable-key-repeat 200 30)
    (setf (sdl:frame-rate) 20)
    (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))
    (setf (game-state g)
          (make-gen :width width
                    :height height
                    :arr (make-array (* width height)
                                     :initial-element dead-cell)))
	;; TODO: remove
	(setf (arr-idx (gen-arr (game-state g)) 1 1 width height) 1)
	(setf (arr-idx (gen-arr (game-state g)) 2 2 width height) 1)
	(setf (arr-idx (gen-arr (game-state g)) 0 3 width height) 1)
	(setf (arr-idx (gen-arr (game-state g)) 1 3 width height) 1)
	(setf (arr-idx (gen-arr (game-state g)) 2 3 width height) 1)
	(game-run g)
	)
  )

;; shut it down
(defmethod game-teardown ((g game))
  (sdl:quit-sdl))

(defmethod game-toggle-paused ((g game))
  (setf (game-paused g) (not (game-paused g))))

;; (defun graphics-init (generation)
;;   (loop for i from 0 to 100
;; 	 do (progn
;; 		  (setf (gen-arr generation)
;; 				(next-gen (gen-arr generation)
;; 						  (gen-width generation)
;; 						  (gen-height generation)))
;; 		  (print-gen generation))))

(defparameter width nil)
(defparameter height nil)
; (defparameter generation nil)

(defmethod game-step ((g game) generation &optional clear)
  (setf (gen-arr generation)
        (next-gen (gen-arr generation)
                  (gen-width generation)
                  (gen-height generation)
				  clear))
  ;TODO
  )

(defmethod game-draw ((g game))
  ;; (print (game-vx g))
  ;; (print (game-vy g))
  (sdl:clear-display sdl:*green*)
  (loop for y from (game-vy g) to (- height 1)
	 do (loop for x from (game-vx g) to (- width 1)
		   do (let ((state (game-state g))
					(y* (- y (game-vy g)))
					(x* (- x (game-vx g))))
				(if (= (arr-idx* (gen-arr state) x y (gen-width state)
								 (gen-height state)) alive-cell)
					(sdl:draw-box (sdl:rectangle
								   :x (scale-xy x*) :y (scale-xy y*)
								   :w *cell-size* :h *cell-size*)
								  :color sdl:*black*))))))

;; set all cells of game-state to deat-cell
(defmethod game-reset ((g game))
  ; TODO
  (game-step g (game-state g) t)
  )

(defmethod game-run ((g game))
  "Game-Of-Life docstring."
  (format t "game-run start~%")
  ; (sdl:update-display)
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
        (if (> (game-vx g) 0) (decf (game-vx g) *config-view-delta*)))
      (when (or (sdl:key= key :sdl-key-d) (sdl:key= key :sdl-key-right))
        (if (<= (game-vx g) (game-width g)) (incf (game-vx g) *config-view-delta*)))
      (when (or (sdl:key= key :sdl-key-w) (sdl:key= key :sdl-key-up))
        (if (> (game-vy g) 0) (decf (game-vy g) *config-view-delta*)))
      (when (or (sdl:key= key :sdl-key-s) (sdl:key= key :sdl-key-down))
        (if (<= (game-vy g) (game-height g)) (incf (game-vy g) *config-view-delta*)))
      ;; zoom view area with [+ -] or [MouseWheel]
      (when (sdl:key= key :sdl-key-minus)
        (when (> *cell-size* 1) ;; minimum of 2x2
          (decf *cell-size* 1)))
      (when (sdl:key= key :sdl-key-equals)
        (when (< *cell-size* (+ *config-window-x* 1)) ;; TODO: square window only?
          (incf *cell-size* 1)))
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
      )
     (:mouse-button-down-event (:button button :x x :y y)
      (format t "mouse button(~a) x(~a) y(~a)~%" button x y)
      (when (sdl:mouse-left-p)
        (mouse-handler g button x y))
      ;(mouse-handler g button x y)
      )
     ;; TODO mouse input
     ;; if no input
     (:idle ()
	  ;; todo
	  ;; (sdl:draw-box (sdl:rectangle :x (scale-xy x) :y (scale-xy y) :w *cell-size* :h *cell-size*)
	  ;; 				:color sdl:*green*)))))
	  (game-draw g)
	  ;; update game at end
	  ;; (sleep .5)
	  (if (not (game-paused g))
		  (game-step g (game-state g)))
	  ;; (sdl:draw-box (sdl:rectangle :x 0 :y 10 :w 200 :h 10)
	  ;; 			  :color (sdl:color))
	  ;; (print-gen (game-state g))
	  (sdl:update-display)
	  )
	 )
  )

(defmethod game-set-cell ((g game) x y value)
  (setf (arr-idx (gen-arr (game-state g)) x y (game-width g) (game-height g)) value)
  )

(defmethod game-get-cell ((g game) x y)
  (arr-idx*  (gen-arr (game-state g)) x y (game-width g) (game-height g)))

(defmethod game-toggle-cell ((g game) x y)
  (if (eql (game-get-cell g x y) dead-cell)
      (game-set-cell g x y alive-cell)
      (game-set-cell g x y dead-cell))
  )

(defun scale-xy (x)
  (* x *cell-size*))

(defun mouse-handler (g button x y)
  (let* ((x (truncate (/ x *cell-size*)))
		 (y (truncate (/ y *cell-size*))))
	(if (and (not (or (>= x (game-width g)) (>= y (game-height g))))
			 (eql button 1))
		(game-toggle-cell g x y))))

(defun main ()
  (print "start of main")
  (if (/= (length *posix-argv*) 3)
	  (print "usage: gol.sh width height")
      (let ((width (parse-integer (cadr *posix-argv*) :junk-allowed t))
            (height (parse-integer (caddr *posix-argv*) :junk-allowed t)))
        (if (or (not (and width height)) (< height 5) (< width 5) (> height 1000) (> width 1000))
			(print "Invalid parameters")
            (let ((g (make-instance 'game))
                  ; (generation (make-gen :width width :height height :arr (make-array (* width height) :initial-element dead-cell)))
                  )
              (game-setup g width height)
              (game-teardown g)
			  ;; (setf (arr-idx (gen-arr generation) 10 10 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 11 10 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 10 11 width height) 1)
			  ;; (setf (arr-idx (gen-arr generation) 11 11 width height) 1)
              ; (print generation)
			  ; (print-gen generation)
			  ; (graphics-init generation)
              )
            )
        )
      )
  )

(sb-int:with-float-traps-masked (:invalid :inexact :overflow)
  (main))
;; (setf (aref generation 4) 1)
;; (print generation)
;; (print (count-neighbors generation 5 0 width height))
