; vim: set autoindent:

(in-package :engine)

;;**************************************************************************************************************
;;board class, engine runs on this
(defclass board-class ()
  ((rows                 :accessor rows)
   (cols                 :accessor cols)
   (water                :accessor water)
   (food-ht              :accessor food              :initform (make-hash-table))
   (board                :accessor board)
   (ants-per-player-ht   :accessor ants-per-player   :initform (make-hash-table))
   (players              :accessor players           :initform nil)
   (player-by-id-ht      :accessor player-by-id-ht   :initform (make-hash-table))
   (players-left         :accessor players-left)
   (path-to-map          :accessor path-to-map       :initarg :path-to-map)
   (n-of-players         :accessor n-of-players)
   (init-n-of-ants       :accessor init-n-of-ants    :initarg :init-n-of-ants)
   (init-n-of-food       :accessor init-n-of-food    :initarg :init-n-of-food)
   (food-per-turn        :accessor food-per-turn     :initarg :food-per-turn)
   (max-turns            :accessor max-turns         :initarg :max-turns)
   (player-classes-list  :accessor player-classes    :initarg :player-classes)))

(defmethod player-ants ((b-class board-class) p-class)
  (gethash p-class (ants-per-player b-class)))

(defmethod player-ants-setf ((b-class board-class) p-class ants)
  (setf (gethash p-class (ants-per-player b-class)) ants))

(defsetf player-ants player-ants-setf)

(defmethod player-by-id ((c board-class) id)
  (gethash id (player-by-id-ht c)))
  
(defmethod check-for-eliminated-players ((c board-class))
  (loop for player in (players c)
       with p-left = 0
       when (> (len player) 0)
       collect player into new-players
       and do (incf p-left)
       finally (setf (players c) new-players
		     (players-left c) p-left)))

(defmethod update-player-ants ((c board-class) list-of-new-ant-vecs)
  ;; (print "new")
  ;; (print list-of-new-ant-vecs)
  ;; (print "first loop")
  ;; (print (player-ants c (first (players c))))

  (loop for player in (players c)
       when (> (len player) 0) do
       (loop for old-ant across (player-ants c player) do
;	    (print old-ant)
	    (setf (aref (board c) (struct-r-c-board-index old-ant)) 0)))

;  (print "second loop")
  (loop for (player new-ant-vec) in list-of-new-ant-vecs do
;       (print player)
       (loop for new-ant across new-ant-vec
	  for new-ant-index from 0 do
	  (setf (aref (board c) (struct-r-c-board-index new-ant))
		(encode-byte (player-b-id player) new-ant-index)))
       (setf (player-ants c player) new-ant-vec)))
  ;; (print "original updated")
  ;; (print (player-ants-original c))
;  (print "third loop")
;  (print "at the end")



  ;; (loop for vec in (player-ants-original c)
  ;; 	 do (print (length vec))))

(defmethod gen-ants ((c board-class) player-id)
  (loop with ant-vec = (make-array 0 :fill-pointer 0 :adjustable t)
     with added = 0
     for row = (random (rows c))
     for col = (random (cols c))
     for board-index = (2-to-1 (rows c) (cols c) row col)
     while (< added (init-n-of-ants c))
     when (= 0 (decode-byte (aref (board c) board-index))) do
     (vector-push-extend (make-struct-r-c :row row :col col :board-index board-index) ant-vec)
     (setf (aref (board c) board-index) (encode-byte player-id added))
     (incf added)
     finally (return ant-vec)))


(defmethod add-n-food ((c board-class) n-of-food)
  (loop with added = 0
     for row = (random (rows c))
     for col = (random (cols c))
     for board-index = (2-to-1 (rows c) (cols c) row col)
     while (< added n-of-food)
     when (= 0 (decode-byte (aref (board c) board-index))) do
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
					 ; (print "mark1") 
  ;;******************
  ;;instantiating player classes
  (loop for player-class-name in (player-classes c)
     for player-id from 1
     for player-b-id from 3	;0-land, 1-water, 2-food, 3-.. players
     for player = (make-instance player-class-name
				 :player-id player-id
				 :player-b-id player-b-id)
     collect player into new-players
     do
     (setf (gethash player-b-id (player-by-id-ht c)) player)
     finally (setf (players c) new-players))
;  (print "mark2")
  ;;******************
  ;;adding initial ants to players
  (loop for player in (players c)
       collect (list player (gen-ants c (player-b-id player))) into new-ant-list
       finally (update-player-ants c new-ant-list))
;  (print "mark3")
  (synchronise-ant-vectors c)
;  (print "mark4")
  (add-n-food c (init-n-of-food c)))


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
	(new-directions (mapcar #'gen-ant-directions (players c))))
    ;; (print "old-dirs:")
    ;; (print new-directions)

    (loop for player in (players c)
       for p-ants = (player-ants c player)
       for directions in new-directions
       collect (loop
		  for direction in directions
		  for ant across p-ants
		  for row = (struct-r-c-row ant)
		  for col = (struct-r-c-col ant)
		  for (n-row n-col) = (multiple-value-list
				       (project-move (rows c) (cols c) row col direction))
		  when (= 1 (decode-byte
			     (aref (board c) (2-to-1 (rows c) (cols c) n-row n-col))))
		  do
		  (setf n-row (struct-r-c-row ant) 
			n-col (struct-r-c-col ant)
			direction :c)
		  do
		  (multiple-value-bind (value found) (gethash `(,n-row ,n-col) new-coords-ht)
		    (if found
			(setf (gethash `(,n-row ,n-col) new-coords-ht) (1+ value))
			(setf (gethash `(,n-row ,n-col) new-coords-ht) 1)))
		  collect direction) into new-dirs
       finally (setf new-directions new-dirs))
	  
	  ;; (format t "~&from ~d:~d to ~d:~d, dir: ~a~%" (struct-r-c-row ant) (struct-r-c-col ant)
	  ;; 		   n-row n-col direction)
	  
    ;; (loop for k being the hash-keys in new-coords-ht using (hash-value v) do
    ;; 		(format t "~&~{~a ~}:~d~%" k v))
    ;; (print "new-dirs:")
    ;; (print new-directions)

    (loop for player in (players c)
       for p-ants = (player-ants c player)
       for directions in new-directions
       collect (list player (loop with new-ants = (make-array 0 :fill-pointer 0 :adjustable t)
       			       for ant across p-ants
       			       for direction in directions
       			       for row = (struct-r-c-row ant)
       			       for col = (struct-r-c-col ant)
       			       for (n-row n-col) = (multiple-value-list
       						    (project-move (rows c) (cols c) row col direction))
       			       do
       			       (multiple-value-bind (value found) (gethash `(,n-row ,n-col) new-coords-ht)
       				 (if (and found (= value 1))
       				     (vector-push-extend
       				      (make-struct-r-c :row n-row :col n-col :board-index (2-to-1 (rows c) (cols c) n-row n-col))
       				      new-ants)))
       			       finally (return new-ants))) into new-ants-vec-list	 
       finally (update-player-ants c new-ants-vec-list))))

    ;; (print "all-new-ants")
    ;; (print all-new-ants)
    ;; (print "end, before update")

    ;; (print "end, after update") ))

    ;; (loop for player in (players c)
    ;;    when player do
    ;;    (player-stats player))

(defmethod resolve-battle ((c board-class))
  (let ((battle-ht (make-hash-table :test 'equal)))
    (loop for player in (players c) do
	 (loop for ant across (player-ants c player)
	    for row = (struct-r-c-row ant)
	    for col = (struct-r-c-col ant)
	    for in-battle-radius = (sweep-board (rows c) (cols c) row col 2 (board c))
	    for enemies = (select in-battle-radius
				  (indicator in-battle-radius
					     (lambda (cell)
					       (and (> (decode-byte cell) 2) 
						    (/= (decode-byte cell) (player-b-id player))))))
	    when (> (length enemies) 0) do
	    (setf (gethash (aref (board c) (struct-r-c-board-index ant)) battle-ht) `(0 ,@enemies))))
    ;; (print "pass1")
    ;; (loop for k being the hash-keys in battle-ht using (hash-value v) do
    ;;       (format t "~&~d: ~{~a ~}~%" k v))

    (loop for k being the hash-keys in battle-ht using (hash-value v)
       for num-of-enemies = (1- (length v)) do
       (mapcar (lambda (b-ind) (incf (first (gethash b-ind battle-ht))
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

    ;; (print "in battleres,ants: long, short, player's:")
    ;; (print (player-ants-original c))
    ;; (print (player-ants c))
    ;; (print (player-ants (first (players c))))

    (loop for player in (players c)
       collect (list player (loop with new-ants = (make-array 0 
							      :fill-pointer 0 
							      :adjustable t)
			       for ant across (player-ants c player)
			       for row = (struct-r-c-row ant)
			       for col = (struct-r-c-col ant) do
			       (multiple-value-bind (value found)
				   (gethash (aref (board c)
						  (2-to-1 (rows c) (cols c) row col)) battle-ht)
				 (declare (ignore value))
				 (if (not found)
				     (vector-push-extend ant new-ants)))
			       finally (return new-ants))) into new-ants-vec-list
	 finally (update-player-ants c new-ants-vec-list))))

    ;; (print "all-new-ants in battle resolution")
    ;; (print all-new-ants)
    

(defmethod spawn-food ((c board-class))
  (let ((new-ants (loop for row being the hash-keys in (food c) using (hash-value column)
		     append (loop for col across column
			       for objects-in-spawn-range = (sweep-board (rows c) (cols c) row col 1 (board c))
			       for ants-in-spawn-range = (mapcar #'decode-byte
								 (select objects-in-spawn-range
									 (indicator objects-in-spawn-range
										    (lambda (cell) (> (decode-byte cell) 2)))))
			       for food-board-index = (2-to-1 (rows c) (cols c) row col)
			       when (> (length ants-in-spawn-range) 0) do
			       (delete-ht row col (food c))
			       (setf (aref (board c) food-board-index) 0)
			       and when (same? ants-in-spawn-range)
			       collect (list (first ants-in-spawn-range)
					     food-board-index row col)))))

    (loop for (id index row col) in new-ants do
	 ;(format t "~&added ant in spawn-food(~d,~d)~%" row col)
	 (setf (aref (board c) index) (encode-byte id (len (player-by-id c id))))
	 (vector-push-extend (make-struct-r-c :row row :col col :board-index index) (player-ants c (player-by-id c id))))))

  ;; (format t "~&num of food: ~d~%" (loop for k being the hash-keys in (food c) using (hash-value v)
  ;;      sum (length v)))

	;; (print "in spawn food, update board with these")
	;; (print update-board-with-these)


;; (print "ants in spawnrange:") (print ants-in-spawn-range)	       

(defmethod synchronise-ant-vectors ((c board-class))
  (loop for player in (players c) do
       (setf (my-ants player) (player-ants c player))))
  ;; (print "per player ants:")
  ;; (print (player-ants c))
;  (print (player-ants (first (players c)))))
  

(defmethod reduce-board ((c board-class))
  ;; (format t "#of ants: ~{~a ~}" (mapcar (lambda (pl) (len pl)) (players c)))
  ;; add one ant only one none is present
;  (add-n-food c (food-per-turn c))
  (msg "advancing ants")
  (advance-ants c)
;  (synchronise-ant-vectors c)
  (msg "resolving battle")
  (resolve-battle c)
;  (synchronise-ant-vectors c)
  (msg "spawn food")
  (spawn-food c)
;  (synchronise-ant-vectors c)
  (check-for-eliminated-players c)
  (synchronise-ant-vectors c)

  (when (= (hash-table-count (food c)) 0)
    (add-n-food c 1))
  (vis:push-draw-event))
