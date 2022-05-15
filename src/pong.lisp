(in-package :pong)

(defparameter *frame-rate* 24)
(defparameter *thickness* 16)
(defparameter *paddle-height* 128)
(defparameter *width* 1024)
(defparameter *height* 768)
(defparameter *ticks-count* 0)
(defparameter *paddle-speed* 230.0)

(defstruct vec2
  x y)

(defstruct game-state
  running-p
  score
  ball-position
  ball-velocity
  paddle-position
  paddle-velocity)

(defun initial-state ()
  (make-game-state
   :running-p nil
   :score 0
   :ball-position (make-vec2 :x (/ *width* 2) :y (/ *height* 2))
   :ball-velocity (make-vec2 :x -200.0 :y 235.0)
   :paddle-position (make-vec2 :x *thickness* :y (/ *height* 2))
   :paddle-velocity (make-vec2 :x 0.0 :y 0.0)))

(defvar *state* (initial-state))

(defun process-input (event-type keysym)
  (case event-type
    (:keyup
     (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	    (sdl2:push-event :quit))
	   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	    (if (game-state-running-p *state*)
		(setf (game-state-running-p *state*) nil)
		(setf (game-state-running-p *state*) t)))
	   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	    (setf (vec2-y (game-state-paddle-velocity *state*)) 0))
	   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	    (setf (vec2-y (game-state-paddle-velocity *state*)) 0))))
    (:keydown
     (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
	    (setf (vec2-y (game-state-paddle-velocity *state*)) (* -1 *paddle-speed*)))
	   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
	    (setf (vec2-y (game-state-paddle-velocity *state*)) *paddle-speed*))))))

(defun update ()
  (let* ((ticks (sdl2:get-ticks))
	 (ticks-delta (- ticks *ticks-count*)))
    (when (< ticks-delta *frame-rate*)
      (sdl2:delay (- *frame-rate* ticks-delta))))

  (let* ((ticks (sdl2:get-ticks))
	 (delta-time (/ (- ticks *ticks-count*) 1000.0)))
    (when (> delta-time 0.05)
      (setf delta-time 0.05))

    (setf *ticks-count* ticks)

    (when (game-state-running-p *state*)
      (update-paddle delta-time)
      (update-ball delta-time))))

(defun delta (position velocity delta-time)
  (let ((delta-x
	  (+ (vec2-x position) (round (* (vec2-x velocity) delta-time))))
	(delta-y
	  (+ (vec2-y position) (round (* (vec2-y velocity) delta-time)))))
    (make-vec2 :x delta-x :y delta-y)))

(defun update-paddle (delta-time)
  (let* ((delta-pos
	   (delta
	    (game-state-paddle-position *state*)
	    (game-state-paddle-velocity *state*)
	    delta-time)))
    (when (and
	   (> (- (vec2-y delta-pos) (/ *paddle-height* 2)) *thickness*)
	   (< (+ (vec2-y delta-pos) (/ *paddle-height* 2)) (- *height* *thickness*)))
      (setf (game-state-paddle-position *state*) delta-pos)
      (setf (sdl2:rect-y *paddle*) (- (vec2-y delta-pos) (/ *paddle-height* 2))))))

(defun update-ball (delta-time)
  (let ((x (vec2-x (game-state-ball-position *state*)))
	(y (vec2-y (game-state-ball-position *state*)))
	(vel-x (vec2-x (game-state-ball-velocity *state*)))
	(vel-y (vec2-y (game-state-ball-velocity *state*))))
    (flet ((flip-ball-direction-x ()
	     (setf (vec2-x (game-state-ball-velocity *state*))
		   (* -1 (vec2-x (game-state-ball-velocity *state*)))))
	   (flip-ball-direction-y ()
	     (setf (vec2-y (game-state-ball-velocity *state*))
		   (* -1 vel-y))))

      (when (and (< y *thickness*) (< vel-y 0))
	(flip-ball-direction-y))

      (when (and (> y (- *height* *thickness*)) (> vel-y 0))
	(flip-ball-direction-y))

      (when (and (> x (- *width* *thickness*)) (> vel-x 0))
	(flip-ball-direction-x))

      (when (and (< x (+ (vec2-x (game-state-paddle-position *state*)) *thickness*))
		 (> y (- (vec2-y (game-state-paddle-position *state*)) (/ *paddle-height* 2)))
		 (< y (+ (vec2-y (game-state-paddle-position *state*)) (/ *paddle-height* 2)))
		 (< vel-x 0))
	(incf (game-state-score *state*) 10)
	(flip-ball-direction-x))

      (when (and (< x 0) (< vel-x 0))
	(decf (game-state-score *state*) 50)
	(when (< (game-state-score *state*) 0)
	  (setf (game-state-score *state*) 0))
	(setf (game-state-running-p *state*) nil)
	(setf (game-state-ball-position *state*)
	      (make-vec2 :x (/ *width* 2) :y (/ *height* 2))))))

  (let ((delta-pos (delta
		    (game-state-ball-position *state*)
		    (game-state-ball-velocity *state*)
		    delta-time)))
    (setf (game-state-ball-position *state*) delta-pos)
    (setf (sdl2:rect-x *ball*) (- (vec2-x delta-pos) (/ *thickness* 2)))
    (setf (sdl2:rect-y *ball*) (- (vec2-y delta-pos) (/ *thickness* 2)))))

(defvar *background* (sdl2:make-rect 0 0 *width* *height*))
(defvar *top-wall* (sdl2:make-rect 0 0 *width* *thickness*))
(defvar *bottom-wall* (sdl2:make-rect 0 (- *height* *thickness*) *width* *thickness*))
(defvar *back-wall* (sdl2:make-rect (- *width* *thickness*) 0 *thickness* *height*))
(defvar *paddle* (sdl2:make-rect *thickness* 0 *thickness* *paddle-height*))
(defvar *ball* (sdl2:make-rect 0 0 *thickness* *thickness*))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 0 0 180 0)
  (sdl2:render-fill-rect renderer *background*)

  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (sdl2:render-fill-rect renderer *top-wall*)
  (sdl2:render-fill-rect renderer *bottom-wall*)
  (sdl2:render-fill-rect renderer *back-wall*)
  (sdl2:render-fill-rect renderer *paddle*)
  (sdl2:render-fill-rect renderer *ball*)

  (sdl2:render-present renderer))

(defun run-game ()
  (setf *ticks-count* 0)
  (update-paddle 0)
  (update-ball 0)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Pong" :w *width* :h *height* :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (process-input :keyup keysym))
	  (:keydown
	   (:keysym keysym)
	   (process-input :keydown keysym))
	  (:idle
	   ()
	   (update)
	   (render renderer))
	  (:quit () t))))))
