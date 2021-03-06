; vim: set autoindent:

(in-package :engine)

;;**************************************************************************************************************
;;dummy player class for testing
(defclass dummy-player ()
  ((my-ants	:accessor my-ants	:initform nil)
   (player-id   :accessor player-id	:initarg :player-id)
   (player-b-id :accessor player-b-id   :initarg :player-b-id)
   (target 	:accessor target)
   (list-to-screen :accessor list-to-screen :initform nil)))

(defmethod len ((c dummy-player))
  (length (my-ants c)))

(defmethod player-stats ((c dummy-player))
  (format t "~&player ~d, board-code ~d with ~d ants~%"
          (player-id c) (player-b-id c) (len c)))

(defmethod gen-ant-directions ((c dummy-player))
  (setf (list-to-screen c) nil)
;  (dump-ht (food *board*))
  (loop for row being the hash-keys in (food *board*) using (hash-value column)
       do
       (setf (target c) `(,row ,(aref column 0)))
       (return))
  
   ;; (print "target:")
   ;; (print (target c))
 ;; (print (target c))
;; (print (my-ants c))

  (loop for ant across (my-ants c)
       for ant-index from 0
       do
       (push `(,(struct-r-c-row ant) ,(struct-r-c-col ant) ,sdl::*red*) (list-to-screen c))
       when (= ant-index 0)
       collect (dir-to-target (struct-r-c-row ant) (struct-r-c-col ant) (first (target c)) (second (target c))) into directions
       else
       collect :c into directions
       finally (return directions)))

;; (defmethod gen-ant-directions ((c dummy-player))
;;   (loop repeat (len c)
;;        collect :c))
;       collect (choose-random '(:n :s :e :w :c))))

(defun dir-to-target (row col t-row t-col)
  (choose-random (choices-from-quadrant row col t-row t-col)))

(defun distance (r1 c1 r2 c2)
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defun distance-list (rc1 rc2)
  (+ (abs (- (first rc1) (first rc2))) (abs (- (second rc1) (second rc2)))))

;; (defun choices-from-quadrant (r1 c1 r2 c2)
;;   (let ((row-delta (- r2 r1))
;;         (col-delta (- c2 c1)))
;; ;    (format t "~&deltas: ~d:~d~%" row-delta col-delta)
;;     (cond ((= 0 row-delta) (if (plusp col-delta)
;;                              '(:e)
;;                              '(:w)))
;;           ((= 0 col-delta) (if (plusp row-delta)
;;                              '(:s)
;;                              '(:n)))
;;           ((and (plusp row-delta) (plusp col-delta)) '(:s :e))
;;           ((and (plusp row-delta) (minusp col-delta)) '(:s :w))
;;           ((and (minusp row-delta) (plusp col-delta)) '(:n :e))
;;           ((and (minusp row-delta) (minusp col-delta)) '(:n :w))
;;           (t nil))))

;; (defun choices-from-quadrant (r1 c1 r2 c2)
;;   (let* ((rows (rows *board*))
;; 	 (cols (cols *board*))
;; 	 (images-of-target `((,(- r2 rows) ,c2)
;; 			     (,(+ r2 rows) ,c2)
;; 			     (,r2 ,(- c2 cols))
;; 			     (,r2 ,(+ c2 cols))))
;; 	 (distances (mapcar (lambda (coord2) (distance-list `(,r1 ,c1) coord2)) images-of-target))
;; 	 (dmin (min distances))
;; 	 (min-image (nth (position dmin distances) images-of-target))
;; 	 (row-delta (- (first min-image) r1))
;; 	 (col-delta (- (second min-image) c1)))
;; 					;(format t "~&deltas: ~d:~d~%" row-delta col-delta)
;;     (cond ((= 0 row-delta) (if (plusp col-delta)
;; 			       '(:e)
;; 			       '(:w)))
;;           ((= 0 col-delta) (if (plusp row-delta)
;; 			       '(:s)
;; 			       '(:n)))
;;           ((and (plusp row-delta) (plusp col-delta)) '(:s :e))
;;           ((and (plusp row-delta) (minusp col-delta)) '(:s :w))
;;           ((and (minusp row-delta) (plusp col-delta)) '(:n :e))
;;           ((and (minusp row-delta) (minusp col-delta)) '(:n :w))
;;           (t nil))))

(defun choices-from-quadrant (r1 c1 r2 c2)
  (let* ((rows (rows *board*))
 	 (cols (cols *board*))
  	 (images-of-target (list (list r2 c2)
				 (list (- r2 rows) c2)
				 (list (+ r2 rows) c2)
				 (list r2 (- c2 cols))
				 (list r2 (+ c2 cols))))
  	 (distances (mapcar (lambda (coord2) (distance-list (list r1 c1) coord2)) images-of-target))
  	 (dmin (apply #'min distances))
  	 (min-image (nth (position dmin distances) images-of-target))
  	 (row-delta (- (first min-image) r1))
  	 (col-delta (- (second min-image) c1)))
    ;; 					;(format t "~&deltas: ~d:~d~%" row-delta col-delta)
    (cond ((= 0 row-delta) (if (plusp col-delta)
  			       '(:e)
  			       '(:w)))
          ((= 0 col-delta) (if (plusp row-delta)
  			       '(:s)
  			       '(:n)))
          ((and (plusp row-delta) (plusp col-delta)) '(:s :e))
          ((and (plusp row-delta) (minusp col-delta)) '(:s :w))
          ((and (minusp row-delta) (plusp col-delta)) '(:n :e))
          ((and (minusp row-delta) (minusp col-delta)) '(:n :w))
          (t nil))))