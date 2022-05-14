(in-package :pong)

(defparameter *frame-rate* 24)
(defparameter *thickness* 16)
(defparameter *paddle-height* 128)
(defparameter *width* 1024)
(defparameter *height* 768)

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

(defun process-input ())

(defun update (game-state))

(defun render ())

(defun run-game ())
