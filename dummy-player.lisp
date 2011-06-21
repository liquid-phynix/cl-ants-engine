; vim: set autoindent:

(in-package :engine)

;;**************************************************************************************************************
;;dummy player class for testing
(defclass dummy-player ()
  ((player-ants :accessor player-ants)
   (player-id   :accessor player-id     :initarg :player-id)
   (board-id    :accessor board-id      :initarg :board-id)))

(defmethod len ((c dummy-player))
  (length (player-ants c)))

(defmethod player-stats ((c dummy-player))
  (format t "~&player ~d, board-code ~d with ~d ants~%"
          (player-id c) (board-id c) (len c)))

(defmethod gen-ant-directions ((c dummy-player))
  (loop repeat (len c)
       collect (choose-random '(:c))))
;        collect (choose-random '(:n :s :e :w :c))))
