; vim: set autoindent:

(in-package :engine)


(defun collect-directions (players water)
  (loop for player in players
        collect (player-directions player water)))

;; meg kene csinalni, hogy ne szamolgasson semmit ha :c az irany
(defun update-board-for-directions (rows cols players players-directions board)
  (let ((move-ht (make-hash-table :test 'equal))
        (stay-ht (make-hash-table :test 'equal)))

    (setf players-directions 
          (loop for player in players
                for directions in players-directions
                collect (loop for direction in directions
                              for ant across (player-ants player)
                              for ant-index from 0
                              for row = (struct-r-c-row ant)
                              for col = (struct-r-c-col ant)
                              for (new-row new-col) = (multiple-value-list (project-move rows cols row col direction))
                              for old-board-index = (2-to-1 rows cols row col)
                              for new-board-index = (2-to-1 rows cols new-row new-col)
                              with value
                              with dirs = (repeat (len player) nil)
                              do
                              ;(setf (aref (board board) old-board-index) 0)
                              (if (or (eq direction :c) (= 1 (decode-byte (aref (board board) new-board-index))))
                                (progn (setf value (gethash `(,row ,col) stay-ht))
                                       (if value
                                         (setf (gethash `(,row ,col) stay-ht) (1+ value))
                                         (setf (gethash `(,row ,col) stay-ht) 1))
                                       (setf (nth ant-index dirs) nil))
                                (progn (setf value (gethash `(,new-row ,new-col) move-ht))
                                       (if value
                                         (setf (gethash `(,new-row ,new-col) move-ht) (1+ value))
                                         (setf (gethash `(,new-row ,new-col) move-ht) 1))
                                       (setf (aref (board board) old-board-index) 0)
                                       (setf (nth ant-index dirs) direction)))
                              finally (return dirs))))


    ;       (print directions)
    ;; (loop for k being the hash-keys in move-ht using (hash-value v)
    ;;    with cnt = 0
    ;;    do
    ;;    (incf cnt)
    ;;    (print (list k v))
    ;;    finally (print cnt))

    ;; (loop for k being the hash-keys in stay-ht using (hash-value v)
    ;;    with cnt = 0
    ;;    do
    ;;    (incf cnt)
    ;;    (print (list k v))
    ;;    finally (print cnt))


    ;; (sleep 2)

    ;    (print players-directions)


    (loop for player in players
          for directions in players-directions
          do

          (loop for ant across (player-ants player)
                for row = (struct-r-c-row ant)
                for col = (struct-r-c-col ant)
                for index from 0
                do
                (setf (aref (board board) (2-to-1 rows cols row col)) 0))


          ;       (print (player-ants player))
          ;       (format t "number of ants before: ~d, and directions: ~d~%" (len player) (length directions))
          (loop 
            for direction in directions
            for row-col across (player-ants player)
            for row = (struct-r-c-row row-col)
            for col = (struct-r-c-col row-col)

            with new-row
            with new-col
            with new-board-index
            with old-board-index
            with ant-vector = (make-array 0
                                          :adjustable t
                                          :fill-pointer 0)
            do
            (cond (direction (multiple-value-setq (new-row new-col) (project-move rows cols row col direction))
                             (setf new-board-index (2-to-1 rows cols new-row new-col)
                                   old-board-index (2-to-1 rows cols row col))
                             (multiple-value-bind (move-value move-found) (gethash `(,new-row ,new-col) move-ht)
                               (multiple-value-bind (stay-value stay-found) (gethash `(,new-row ,new-col) stay-ht)
                                 (if stay-found
                                   (progn
                                     (setf (aref (board board) old-board-index) 0))
                                   (if (> move-value 1)
                                     (setf (aref (board board) old-board-index) 0)
                                     (progn 
                                       ;;(setf (aref (board board) new-board-index)
                                       ;;      (encode-byte (player-id player) (length ant-vector)))
                                       (setf (aref (board board) old-board-index) 0)
                                       (vector-push-extend
                                         (make-struct-r-c :row new-row :col new-col)
                                         ant-vector)))))))
                  (t (setf old-board-index (2-to-1 rows cols row col))
                     (multiple-value-bind (value found) (gethash `(,row ,col) move-ht)
                       (if (not found)
                         (progn 
                           ;; (setf (aref (board board) old-board-index)
                           ;; 	 (encode-byte (player-id player) (length ant-vector)))
                           (vector-push-extend
                             (make-struct-r-c :row row :col col)
                             ant-vector))
                         (setf (aref (board board) old-board-index) 0)))))

            finally (setf (player-ants player) ant-vector))

          ;       (print (player-ants player))

          (loop for ant across (player-ants player)
                for row = (struct-r-c-row ant)
                for col = (struct-r-c-col ant)
                for index from 0
                do
                (setf (aref (board board) (2-to-1 rows cols row col))
                      (encode-byte (player-id player) index)))


          )))
;  (vis:push-draw-event))



(defun check-for-battle (rows cols players board)
  (loop
    with enemies-hashtable = (make-hash-table)
    for player in players
    do
    (loop for ant-struct across (player-ants player)
          for row = (struct-r-c-row ant-struct)
          for col = (struct-r-c-col ant-struct)
          for sweep = (coerce (sweep-board rows cols row col 2 (board board)) 'list)
          for ids = (mapcar #'decode-byte sweep)
          for enemy-board-indices = (coerce 
                                      (select sweep (indicator ids (lambda (arg)
                                                                     (and
                                                                       (> arg 2)
                                                                       (/= arg (player-id player))))))
                                      'vector)
          when (/= 0 (length enemy-board-indices))
          do
          (setf (gethash (aref (board board) (2-to-1 rows cols row col)) enemies-hashtable)
                (list 0 enemy-board-indices)))
    finally (return enemies-hashtable)))

(defun check-for-food-spawn (rows cols food players board)
  ; (print "in chech for food")
  ; (print (type-of (player-ants (first players))))
  (loop for row being the hash-keys in food using (hash-value columns)
        do
        (loop for col across columns
              for sweep = (coerce (sweep-board rows cols row col 1 (board board)) 'list)
              for ids = (mapcar #'decode-byte sweep)
              for ants-near-food = (select sweep (indicator ids (lambda (arg) (> arg 2))))
              do
              (when (> (length ants-near-food) 0)
                (if (same? (mapcar #'decode-byte ants-near-food))
                  (let ((ant-id (decode-byte (first ants-near-food))))
                    (vector-push-extend (make-struct-r-c :row row :col col)
                                        (player-ants (nth (- ant-id 3) players)))
                    (extend-queue board row col (encode-byte ant-id (len (nth (- ant-id 3) players)))))
                  (extend-queue board row col 0))
                (delete-ht row col food)))))

(defun distribute-damage (players-enemies-hashtable)
  ;;  (print "ht:")
  ;;  (dump-ht players-enemies-hashtable)
  (loop for k being the hash-keys in players-enemies-hashtable using (hash-value v)
        for enemies = (length (second v))
        do
        (map 'vector
             (lambda (h-key)
               (gethash h-key players-enemies-hashtable)
               (incf
                 (first (gethash h-key players-enemies-hashtable))
                 (/ 1 enemies)))
             ;; (lambda (h-key)
             ;;   (when (gethash h-key players-enemies-hashtable)
             ;;     (incf
             ;;      (first (gethash h-key players-enemies-hashtable))
             ;;      (/ 1 enemies))))
             (second v))))

(defun select-ants-to-die (players-enemies-hashtable players-untouched board)
  (let ((ants-per-players-to-be-deleted (repeat (length players-untouched) nil)))
    (loop for k being the hash-keys in players-enemies-hashtable using (hash-value v)
          with ant-struct 
          when (>= (first v) 1)
          do
          (multiple-value-bind (id index) (decode-byte k)
            (push index (nth (- id 3) ants-per-players-to-be-deleted))
            (when *debug* (format t "~&id: ~d, ants: ~d, index: ~d~%" id (len (nth (- id 3) players-untouched)) index))
            (setf ant-struct
                  (aref (player-ants (nth (- id 3) players-untouched)) index))
            (extend-queue board (struct-r-c-row ant-struct) (struct-r-c-col ant-struct) 0)))

    (loop for player in players-untouched
          for p-ind from 0
          when (/= 0 (len player))
          do
          (let ((to-be-deleted (sort (nth p-ind ants-per-players-to-be-deleted)  #'<))
                (ant-struct-vector (player-ants player))
                (ant-struct-fill-vector (make-array 0 :fill-pointer 0 :adjustable t)))
            (when to-be-deleted
              (loop for index below (len player)
                    when (or (null to-be-deleted)
                             (/= index (first to-be-deleted)))
                    do
                    (vector-push-extend (aref ant-struct-vector index) ant-struct-fill-vector)
                    else
                    do (pop to-be-deleted))
              (setf (player-ants player) ant-struct-fill-vector)))

          (loop for ant across (player-ants player)
                for ind from 0
                do
                (extend-queue board (struct-r-c-row ant) (struct-r-c-col ant) (encode-byte (player-id player) ind))))))


