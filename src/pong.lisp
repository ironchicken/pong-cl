(in-package :pong)

(defparameter *frame-rate* 24)
(defparameter *thickness* 16)
(defparameter *paddle-height* 128)
(defparameter *width* 1024)
(defparameter *height* 768)
(defparameter *ticks-count* 0)

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
   :running-p t
   :score 0
   :ball-position (make-vec2 :x (/ *width* 2) :y (/ *height* 2))
   :ball-velocity (make-vec2 :x -200.0 :y 235.0)
   :paddle-position (make-vec2 :x *thickness* :y (/ *height* 2))
   :paddle-velocity (make-vec2 :x 0.0 :y 0.0)))

(defvar *state* (initial-state))

(defun process-input (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit)))

(defun update ()
  (let* ((ticks (sdl2:get-ticks))
	 (ticks-delta (- ticks *ticks-count*)))
    (when (< ticks-delta *frame-rate*)
      (sdl2:delay ticks-delta)))

  (let* ((ticks (sdl2:get-ticks))
	 (delta-time (/ (- ticks *ticks-count*) 1000.0)))
    (when (> delta-time 0.05)
      (setf delta-time 0.05))

    (setf *ticks-count* ticks)))

(defvar *background* (sdl2:make-rect 0 0 *width* *height*))
(defvar *top-wall* (sdl2:make-rect 0 0 *width* *thickness*))
(defvar *bottom-wall* (sdl2:make-rect 0 (- *height* *thickness*) *width* *thickness*))
(defvar *back-wall* (sdl2:make-rect (- *width* *thickness*) 0 *thickness* *height*))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 0 0 180 0)
  (sdl2:render-fill-rect renderer *background*)

  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (sdl2:render-fill-rect renderer *top-wall*)
  (sdl2:render-fill-rect renderer *bottom-wall*)
  (sdl2:render-fill-rect renderer *back-wall*)

  (sdl2:render-present renderer))

(defun run-game ()
  (setf *ticks-count* 0)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Pong" :w *width* :h *height* :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (process-input keysym))
	  (:idle
	   ()
	   (update)
	   (render renderer))
	  (:quit () t))))))
