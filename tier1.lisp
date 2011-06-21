; vim: set autoindent:

(in-package :engine)

(defclass logic-1 (player-sc)
  ((new-directions :accessor new-directions :initform nil)))

(defmethod player-start-turn ((c logic-1))
  (setf (new-directions c)
        (gen-cycle-directions c)))

(defmethod gen-cycle-directions ((c logic-1))
  (flet ((wave (arg)
               (floor (+ 1 (* 10 (expt (sin (* pi (/ 1.0 150) arg)) 2))))))
    (let ((o1 (lambda (tm) (wave tm)))
          (o2 (lambda (tm) (wave (+ tm (/ 150 4)))))
          (o3 (lambda (tm) (wave (+ tm (* 2 (/ 150 4))))))
          (o4 (lambda (tm) (wave (+ tm (* 3 (/ 150 4)))))))
      (append (repeat (funcall o1 (turn *turn0*)) :north)
              (repeat (funcall o2 (turn *turn0*)) :east)
              (repeat (funcall o3 (turn *turn0*)) :south)
              (repeat (funcall o4 (turn *turn0*)) :west)))))

(defmethod gen-ant-directions ((c logic-1))
  ;;;  (log-msg "ants:" (ant-count c))
  (let* ((assoc-vec (make-array (ant-count c) :initial-element nil))
         (ant-copy (copy-seq (player-ants-vec c)))
         (remaining-ants (ant-count c))
         (food+enemies (make-array (+ (length (food c)) (length (enemy-ants-vec c)))
                                   :fill-pointer (+ (length (food c)) (length (enemy-ants-vec c)))
                                   :adjustable t
                                   :initial-contents (merge 'vector (food c) (enemy-ants-vec c) (lambda (a1 a2) nil)))))
    ;;        (food+enemies (make-array (+ (length (food c)) 0)
    ;;                                  :fill-pointer (length (food c))
    ;;                                  :adjustable t
    ;;                                  :initial-contents (food c))))
    ;;    (print-stderr food+enemies)
    (loop with fe
          with closest-index
          with row
          with col
          with choice
          for remaining-fe = (length food+enemies)
          while (and (> remaining-fe (if (<= (ant-count c) 200) (length (enemy-ants-vec c)) 0)) 
                     (> remaining-ants 0))
          do
          (setf fe (vector-pop food+enemies))
          (setf closest-index (find-closest fe ant-copy))
          ;          (log-msg "closestindex")
          ;          (print-stderr closest-index)
          (setf row (struct-r-c-row (aref (player-ants-vec c) closest-index)))          
          (setf col (struct-r-c-col (aref (player-ants-vec c) closest-index)))
          (setf choice
                (choose-randomly (filter (choices-from-quadrant row col
                                                                (struct-r-c-row fe) (struct-r-c-col fe))
                                         (forbidden-directions c row col))))
          (when choice (add-to-excluded c
                                        (p-d-row row choice)
                                        (p-d-col col choice)))

          (setf (aref assoc-vec closest-index) choice)
          (decf remaining-ants))

    ;    (log-msg "assoc:")
    ;    (print-stderr assoc-vec)

    (loop for ant across (player-ants-vec c)
          for prev-decision across assoc-vec
          for index from 0
          for row = (struct-r-c-row ant)
          for col = (struct-r-c-col ant)
          for forbidden-dirs = (forbidden-directions c row col)
          for cycled-free-dirs = (filter (new-directions c) forbidden-dirs)
          for chosen-cycled = (choose-randomly cycled-free-dirs)
          for chosen-mean = (choose-randomly
                              (filter                         
                                (multiple-value-bind (r1 c1) 
                                  (mean-position c) 
                                  (append (extend-list (choices-from-quadrant r1 c1 row col) 10) cycled-free-dirs))
                                forbidden-dirs))
          do
          (if (not prev-decision)
            (if (or (< (ant-count c) 3) (> (turn *turn0*) 100))
              (progn (setf (aref assoc-vec index) chosen-cycled)
                     (when chosen-cycled (add-to-excluded c
                                                          (p-d-row row (aref assoc-vec index))
                                                          (p-d-col col (aref assoc-vec index)))))
              (progn (setf (aref assoc-vec index) chosen-mean)
                     (when chosen-mean (add-to-excluded c
                                                        (p-d-row row (aref assoc-vec index))
                                                        (p-d-col col (aref assoc-vec index))))))))
    (coerce assoc-vec 'list)))


(defun extend-list (inlist num)
  (loop repeat num
        with outlist
        do
        (setf outlist (append outlist inlist))
        finally (return outlist)))


(defun distance (r1 c1 r2 c2)
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defun choices-from-quadrant (r1 c1 r2 c2)
  (let ((row-delta (- r2 r1))
        (col-delta (- c2 c1)))
    (cond ((= 0 row-delta) (if (plusp col-delta)
                             '(:east)
                             '(:west)))
          ((= 0 col-delta) (if (plusp row-delta)
                             '(:south)
                             '(:north)))
          ((and (plusp row-delta) (plusp col-delta)) '(:south :east))
          ((and (plusp row-delta) (minusp col-delta)) '(:south :west))
          ((and (minusp row-delta) (plusp col-delta)) '(:north :east))
          ((and (minusp row-delta) (minusp col-delta)) '(:north :west))
          (t nil))))

(defun find-closest (s-food s-ant-vec)
  (let ((r1 (struct-r-c-row s-food))
        (c1 (struct-r-c-col s-food))
        (shortest-index))
    (setf shortest-index (loop for s-ant across s-ant-vec
                               for index from 0
                               with r2
                               with c2
                               with dist
                               with shortest = 1000000
                               with shortest-index = nil
                               when s-ant
                               do
                               (setf r2 (struct-r-c-row s-ant)
                                     c2 (struct-r-c-col s-ant)
                                     dist (distance r1 c1 r2 c2))
                               when (and s-ant (> shortest dist))
                               do
                               (setf shortest dist
                                     shortest-index index)
                               finally (return shortest-index)))
    (when shortest-index (setf (aref s-ant-vec shortest-index) nil))
    shortest-index))

