; vim: set filetype=lisp autoindent:

(in-package :engine)

; "/home/mcstar/src/aiall/engine/symmetric_2.map"
; "/home/mcstar/src/aiall/engine/symmetric_10.map"
; "/home/mcstar/src/aiall/engine/empty.map"

(defmethod run-engine ((c board-class))
  (let ((turn 0) (scale 8) (rows (rows c)) (cols (cols c)) (vis-thread nil)
        (colors (gen-colors (n-of-players c))))
    (setf *scale* scale)

    ;;****************************************************************
    ;; setting up visualzer thread
    (vis:init-vis rows (if *dview* (+ (* 2 cols) 1) cols)  "Ants: The Game" scale)
    (setf vis-thread
          (vis:with-threaded-vis
            (vis:clear)
            (show-board rows cols colors (board c))
            (vis:put-text 0 0 "turn:" :val turn :j :left)
	    (when *dview*
	      (sdl::draw-vline (* cols scale) 0 (1- (* rows scale)) :color sdl::*white*)
;	      (sdl::draw-vline (random (* cols scale)) 0 (random (1- (* rows scale))) :color sdl::*white*)
	      (per-user-plot))
            (diplay-player-stats rows cols scale (players c))))
    ;;****************************************************************
    (msg "turn:" turn)
    (vis:push-draw-event)
    (sleep 0.1)

    ;;****************************************************************
    ;; main game loop
    (do ()
	((or (>= turn (max-turns c))
	     (not (sb-thread:thread-alive-p vis-thread))
	     (= 0 (players-left c))))
      (incf turn)
      (msg "turn:" turn)
      ;; (format t "~&turn: ~d~%" turn)
      ;; reduce game state in each turn
      (reduce-board c)
      ;; (format t "~&turn ~d is over~%" turn)
      ;; do we still have players left?
      ;; (when (= (players-left c) 0)
      ;;   (msg "no more players, terminating"))
      (vis:push-draw-event)
      (sleep 0.1))
    ;;****************************************************************
    (vis:end)))


(defun start-engine (&key
		     (m "/home/mcstar/src/aiall/engine/empty.map")
		     (players 1) (ants-per-player 1)
		     (max-turns 10000) (dbg nil)
		     (food 0) (food-per-turn 0) (dview nil))
  ;; (with-open-file (file-stream #p"/home/mcstar/randomstate"
  ;; 			       :direction :input)
  ;;   (setf *random-state* (read file-stream)))
  (setf *random-state* (sb-ext:seed-random-state t))
  (setf *debug* dbg)
  (setf *dview* dview)
  (setf *board* (make-instance 'board-class
			       :path-to-map m
			       :init-n-of-ants ants-per-player
			       :init-n-of-food food
			       :food-per-turn food-per-turn
			       :max-turns max-turns
			       :player-classes (repeat players 'dummy-player)))
  (run-engine *board*))
