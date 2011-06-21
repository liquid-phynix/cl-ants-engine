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

;;**************************************************************************************************************
;;board class, engine runs on this
(defclass board-class ()
  ((rows                :accessor rows)
   (cols                :accessor cols)
   ;  (update-queue :accessor update-queue :initform nil)
   (water                :accessor water)
   (food-ht              :accessor food              :initform (make-hash-table))
   (board                :accessor board)
   (player-ants          :accessor player-ants       :initform nil)
   (player-ants-original :accessor player-ants-original       :initform nil)
   (players              :accessor players           :initform nil)
   (players-original     :accessor players-original  :initform nil)
   (players-left         :accessor players-left)
   (path-to-map          :accessor path-to-map       :initarg :path-to-map)
   (n-of-players         :accessor n-of-players)
   (init-n-of-ants       :accessor init-n-of-ants    :initarg :init-n-of-ants)
   (init-n-of-food       :accessor init-n-of-food    :initarg :init-n-of-food)
   (food-per-turn        :accessor food-per-turn     :initarg :food-per-turn)
   (max-turns            :accessor max-turns         :initarg :max-turns)
   (player-classes-list  :accessor player-classes    :initarg :player-classes)))

(defmethod player-by-id ((c board-class) num)
  (nth num (players-original c)))

(defmethod ants-by-id ((c board-class) num)
  (nth num (player-ants-original c)))

(defmethod update-player-ants ((c board-class) list-of-new-ant-vecs)
  ;; (print "old:")
  ;; (print  (player-ants-original c))
  ;; (print "new")
  ;; (print list-of-new-ant-vecs)
  (loop for old-ant-vec in (player-ants c) do
       (loop for old-ant across old-ant-vec do
	    (setf (aref (board c) (struct-r-c-b-ind old-ant)) 0)))

  (loop for (b-id new-ant-vec) in list-of-new-ant-vecs do
       (loop for new-ant across new-ant-vec
	  for new-ant-index from 0 do
	  (setf (aref (board c) (struct-r-c-b-ind new-ant))
		(encode-byte b-id new-ant-index)))
       (setf (nth b-id (player-ants-original c)) new-ant-vec))

  (loop for ant-vec in (player-ants-original c)
     for b-id from 0
     with out = nil
     when ant-vec do
     (push ant-vec out)
     (setf (player-ants (nth b-id (players-original c))) ant-vec)
     finally (setf (player-ants c) (reverse out))))

  ;; (loop for vec in (player-ants-original c)
  ;; 	 do (print (length vec))))

(defmethod add-n-ants ((c board-class) p-ind)
  (let ((ant-vec (make-array 0 :fill-pointer 0 :adjustable t)))
    (loop
       with added = 0
       for row = (random (rows c))
       for col = (random (cols c))
       for board-index = (2-to-1 (rows c) (cols c) row col)
       while (< added (init-n-of-ants c))
       when (= 0 (decode-byte (aref (board c) board-index)))
       do
       (vector-push-extend (make-struct-r-c :row row :col col :b-ind board-index) ant-vec)
       (setf (aref (board c) board-index) (encode-byte p-ind added))
       (incf added))
    ant-vec))

(defmethod add-n-food ((c board-class) n-of-food)
  (loop
     with added = 0
     for row = (random (rows c))
     for col = (random (cols c))
     for board-index = (2-to-1 (rows c) (cols c) row col)
     while (< added n-of-food)
     when (= 0 (decode-byte (aref (board c) board-index)))
     do
     (extend-ht row col (food c))
     (setf (aref (board c) board-index) (encode-byte 2 0))
     (incf added)))

(defmethod initialize-instance :after ((c board-class) &rest rest)
  (declare (ignore rest))
  ;;******************
  ;;loading map, setting up water and board
  (multiple-value-bind (rows cols water board) (load-map (path-to-map c))
    (setf (rows c) rows
	  (cols c) cols
	  (water c) water
	  (board c) board))
  (setf (n-of-players c) (length (player-classes c))
	(players-left c) (n-of-players c))
					;  (print "mark1") 
  ;;******************
  ;;instantiating player classes
  (setf (players c) (loop for player-class-name in (player-classes c)
		       for player-id from 1
		       for board-id from 3 ;0-land, 1-water, 2-food, 3-.. players
		       collect (make-instance player-class-name
					      :player-id player-id
					      :board-id board-id)))
  (setf (players-original c) (append '(nil nil nil) (copy-list (players c))))
					;  (print "mark2")
  ;;******************
  ;;adding initial ants to players
  (loop for player in (players c) do
       (push (add-n-ants c (board-id player)) (player-ants c))
       (setf (player-ants player) (first (player-ants c)))
       finally (setf (player-ants c) (reverse (player-ants c))))
  (setf (player-ants-original c) (append '(nil nil nil) (copy-list (player-ants c))))
					;  (print "mark3")
  (add-n-food c (init-n-of-food c))
					;  (print "mark4")

  ;; (print "player class names")
  ;; (print (player-classes c))

  (run-engine c))



  ;;(defmethod extend-queue ((c board-class) row col value)
  ;;  (push (list row col value) (update-queue c)))
  ;;
  ;;(defmethod execute-update ((c board-class))
  ;;  (loop for (row col value) in (update-queue c)
  ;;        do
  ;;        (setf
  ;;          (aref (board c) (2-to-1 (rows c) (cols c) row col))
  ;;          value))
  ;;  (setf (update-queue c) nil))

(defmethod advance-ants ((c board-class))
  (let ((new-coords-ht (make-hash-table :test 'equal))
	all-directions all-new-ants)
    (setf all-directions
	  (loop for player in (players c)
	     collect (gen-ant-directions player)))
    ;; (print "directions:")
    ;; (print all-directions)
    (loop for player in (players c)
       for directions in all-directions
       do
       (loop 
	  for d-ind from 0
	  for direction in directions
	  for ant across (player-ants player)
	  for (n-row n-col) = (multiple-value-list
			       (project-move (rows c)
					     (cols c) 
					     (struct-r-c-row ant) 
					     (struct-r-c-col ant) 
					     direction))
	  do
	  (when (= 1 (decode-byte
		      (aref (board c)
			    (2-to-1 (rows c) (cols c) n-row n-col))))
	    (setf n-row (struct-r-c-row ant) 
		  n-col (struct-r-c-col ant)
		  (nth d-ind directions) :c))
	  ;; (format t "~&from ~d:~d to ~d:~d, dir: ~a~%" (struct-r-c-row ant) (struct-r-c-col ant)
	  ;; 		   n-row n-col direction)
	  (multiple-value-bind (value found) (gethash `(,n-row ,n-col) new-coords-ht)
	    (if found
		(setf (gethash `(,n-row ,n-col) new-coords-ht) (1+ value))
		(setf (gethash `(,n-row ,n-col) new-coords-ht) 1)))))
    ;;do
    ;; (loop for k being the hash-keys in new-coords-ht-per-player using (hash-value v) do
    ;; 		(format t "~&~{~a ~}:~d~%" k v))
    (setf all-new-ants
	  (loop for player in (players c)
	     for directions in all-directions
	     collect (list
		      (board-id player)
		      (loop 
			 with new-ants = (make-array 0 :fill-pointer 0 :adjustable t)
			 for ant across (player-ants player)
			 for direction in directions
			 for row = (struct-r-c-row ant)
			 for col = (struct-r-c-col ant)
			 for (n-row n-col) = (multiple-value-list
					      (project-move (rows c)
							    (cols c)
							    (struct-r-c-row ant)
							    (struct-r-c-col ant)
							    direction))
			 do
			 (multiple-value-bind (value found) (gethash `(,n-row ,n-col) new-coords-ht)
			   (if (and found (= value 1))
			       (vector-push-extend 
				(make-struct-r-c :row n-row :col n-col :b-ind (2-to-1 
									       (rows c) 
									       (cols c) 
									       n-row 
									       n-col))
				new-ants)))
			 finally (return new-ants)))))

    (update-player-ants c all-new-ants)))

    ;; (loop for player in (players c)
    ;;    when player do
    ;;    (player-stats player))

(defmethod resolve-battle ((c board-class))
  (let ((battle-ht (make-hash-table :test 'equal))
	all-new-ants)
    (loop for ant-vec in (player-ants c)
       for player-index from 0
       do
       (loop for ant across ant-vec
	  for row = (struct-r-c-row ant)
	  for col = (struct-r-c-col ant)
	  for in-battle-radius = (sweep-board (rows c) (cols c) row col 2 (board c))
	  for enemies = (select
			 in-battle-radius 
			 (indicator 
			  in-battle-radius 
			  (lambda (cell)
			    (and (> (decode-byte cell) 2) 
				 (/= (decode-byte cell) (board-id (nth player-index (players c))))))))
	  when (> (length enemies) 0) do
	  (setf (gethash (aref (board c) (struct-r-c-b-ind ant)) battle-ht) `(0 ,@enemies))))
    ;; (print "pass1")
    ;; (loop for k being the hash-keys in battle-ht using (hash-value v) do
    ;;       (format t "~&~d: ~{~a ~}~%" k v))

    (loop for k being the hash-keys in battle-ht using (hash-value v)
       for num-of-enemies = (1- (length v))
       do
       (mapcar (lambda (b-ind)
		 (incf (first (gethash b-ind battle-ht))
		       (/ 1 num-of-enemies)))
	       (rest v)))
    ;; (print "pass2")
    ;; (loop for k being the hash-keys in battle-ht using (hash-value v) do
    ;;       (format t "~&~d: ~{~a ~}~%" k v))

    (loop for k being the hash-keys in battle-ht using (hash-value v)
       when (< (first v) 1) do
       (remhash k battle-ht))

    ;; (print "pass3")
    ;; (loop for k being the hash-keys in battle-ht using (hash-value v) do
    ;;       (format t "~&~d: ~{~a ~}~%" k v))

    (setf all-new-ants
	  (loop for player in (players c)
	     collect (list 
		      (board-id player) 
		      (loop 
			 with new-ants = (make-array 0 :fill-pointer 0 :adjustable t)
			 for ant across (player-ants player)
			 for row = (struct-r-c-row ant)
			 for col = (struct-r-c-col ant)
			 do
			 (multiple-value-bind 
			       (value found)
			     (gethash (aref (board c) (2-to-1 (rows c) 
							      (cols c)
							      row 
							      col)) battle-ht)
			   (declare (ignore value))
			   (if (not found)
			       (vector-push-extend ant new-ants)))
			 finally (return new-ants)))))
    (update-player-ants c all-new-ants)))

(defmethod spawn-food ((c board-class))
  (let (update-board-with-these)
    (setf update-board-with-these
	  (loop for row being the hash-keys in (food c) using (hash-value column)
	     append (loop with new-ant
		       with new-ant-b-id
		       for col across column
		       for objects-in-spawn-range  = (sweep-board (rows c) (cols c)
								  row col 1 (board c))
		       for ants-in-spawn-range  = (mapcar #'decode-byte 
							  (select
							   objects-in-spawn-range
							   (indicator
							    objects-in-spawn-range
							    (lambda (cell)
							      (> (decode-byte cell) 2)))))
		       for food-board-index = (2-to-1 (rows c) (cols c) row col)
		       when (> (length ants-in-spawn-range) 0) do
		       (delete-ht row col (food c))
		       (setf (aref (board c) food-board-index) 0)
		       and when (same? ants-in-spawn-range) do
		       (setf new-ant (make-struct-r-c :row row :col col :b-ind food-board-index)
			     new-ant-b-id (first ants-in-spawn-range))
		       (vector-push-extend new-ant (nth new-ant-b-id (player-ants-original c)))
		       and collect (list food-board-index 
					 (encode-byte new-ant-b-id
						      (length 
						       (nth new-ant-b-id 
							    (player-ants-original c))))))))
    (loop for lst in update-board-with-these do
	 (setf (aref (board c) (first lst))
	       (second lst)))))

;; (print "ants in spawnrange:") (print ants-in-spawn-range)	       



(defmethod reduce-board ((c board-class))
  (add-n-food c (food-per-turn c))
  (advance-ants c)
  (resolve-battle c)
  (spawn-food c))
