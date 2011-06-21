; vim: set filetype=lisp autoindent:

(in-package :engine)

(declaim (inline 2-to-1 1-to-2)
         (notinline wrap))

;;*****************************************************************************************************
;;very general helpers

(defun encode-byte (id index)
  (declare (type cell id index))
  (logxor (ash id 22) index))

(defun decode-byte (cell-value)
  (declare (type cell cell-value))
  (let ((id (ash cell-value -22)))
    (values id
            (logxor (ash id 22) cell-value))))

(defmacro same? (arg)
  `(if (> (length ,arg) 1)
     (reduce (lambda (arg1 arg2) (and arg1 arg2))
             (mapcar #'= ,arg (rest ,arg)))
     t))

(defun wrap (var len)
  ;; (declare (type cell len)
  ;;          (type (signed-byte 32) var))
  (floor (if (< var 0) 
	     (wrap (+ var len) len) 
	     (if (>= var len) 
		 (wrap (- var len) len) 
		 var))))

(defun 2-to-1 (rows cols row col)
  (+ (wrap col cols) (* (wrap row rows) cols)))

(defun 1-to-2 (rows cols index)
  (declare (ignore rows))
  (truncate index cols))

(defmacro repeat (num arg)
  `(loop repeat ,num
         collect ,arg))

(defun split-name-value (instring)
  (let* ((spaces
           (append
             (loop for ind from 0 to (1- (length instring))
                   when (char= (char instring ind) #\space) collect ind)
             '(nil))))
    (cons
      (subseq instring 0 (first spaces))
      (loop for item in (mapcar
                          (lambda (i1 i2)
                            (parse-integer instring :junk-allowed t :start i1 :end i2))
                          spaces (rest spaces))
            when item
            collect item))))

(defun indicator (inlist testfun)
  (loop for x in inlist
        collect (funcall testfun x)))

(defun select (inlist indicatorlist)
  (loop for el in inlist
        for indic in indicatorlist
        when indic collect el))

(defun choose-random (choices)
  (let ((num-of-choices (length choices)))
    (cond ((= num-of-choices 0) nil)
          ((= num-of-choices 1) (first choices))
          (t (nth (random num-of-choices) choices)))))

;;*****************************************************************************************************
;;less general helpers

;;************************************************
;;regarding hashtables
(defun dump-ht (h)
  (loop
    for k being the hash-keys in h using (hash-value v)
    do (format *error-output* "row:col - ~d: ~{~a~^, ~}~%" k (coerce v 'list))))

(defmacro mk-ht ()
  `(make-hash-table))

(defmacro ht-len (ht)
  `(hash-table-count ,ht))

(defun extend-ht (row col ht)
  (if (gethash row ht)
    (vector-push-extend col (gethash row ht))
    (setf (gethash row ht)
          (make-array 1 :initial-element col :fill-pointer 1 :adjustable t))))

(defun part-of-ht (row col ht)
  (let ((column (gethash row ht)))
    (if column
      (loop for c across column
            when (= c col)
            do (return t))
      nil)))

(defun delete-ht (row col ht)
  (let ((temp (make-array 0 :fill-pointer 0 :adjustable t )))
    (if (gethash row ht)
      (progn (loop for c across (gethash row ht)
                   when (/= c col)
                   do
                   (vector-push-extend c temp))
             (if (= (length temp) 0)
               (remhash row ht)
               (setf (gethash row ht) temp)))
      nil)))
;;************************************************

(defun msg (message &optional (parameter ""))
  (when *debug*
    (format t "~&-- ~a ~a --~%" message parameter)))

;;(defmacro init-array (&optional initarg)
;;  (if initarg
;;    `(make-array 16384 :fill-pointer 1 :initial-element ,initarg)
;;    `(make-array 16384 :fill-pointer 0)))

(defun load-map (filename)
  (let ((rows 0) (cols 0) (water nil) (board nil))
    (with-open-file (f-s (merge-pathnames filename) :direction :input)
      (setf rows (second (split-name-value (read-line f-s nil)))
            cols (second (split-name-value (read-line f-s nil))))
      (read-line f-s nil)
      (setf water (make-array 0 :fill-pointer 0 :adjustable t))
      (setf board (make-array (* rows cols)
                              :initial-element (the cell 0)
                              :element-type 'cell))
      (loop for (full-line end) = (multiple-value-list (read-line f-s nil))
            with board-index = 0
            with water-index = 0
            with row
            with col
            while (not end) do
            (loop for char across (subseq full-line 2)
                  when (char= char #\%)
                  do
                  (multiple-value-setq (row col)
                    (1-to-2 rows cols board-index))
                  (setf (aref board board-index) (encode-byte 1 water-index))
                  (incf water-index)
                  (vector-push-extend (make-struct-r-c :row row :col col) water)
                  do
                  (incf board-index))))
    (values rows cols water board)))



;;(defun count-visible-ants (rows cols row col board)
;;  (declare (type cell rows cols row col))
;;  (let ((reg1 (gen-cat-1-indices rows cols row col))
;;        (reg2 (gen-cat-2-indices rows cols row col))
;;        (reg3 (gen-cat-3-indices rows cols row col)))
;;    (loop for reg in (list reg1 reg2 reg3)
;;          sum (loop for index across reg
;;                    for id = (decode-byte (aref board index))
;;                    when (> id 2)
;;                    sum 1))))

(defun sweep-board (rows cols row col reg board)
  (flet ((by-ind (ind) (aref board ind)))
    (cond ((= reg 1) (mapcar #'by-ind (coerce (gen-cat-1-indices rows cols row col) 'list)))
          ((= reg 2) (append (sweep-board rows cols row col 1 board) (mapcar #'by-ind (coerce (gen-cat-2-indices rows cols row col) 'list))))
          ((= reg 3) (append (sweep-board rows cols row col 2 board) (mapcar #'by-ind (coerce (gen-cat-3-indices rows cols row col) 'list)))))))


(defun project-move (rows cols row col direction)
  (declare (type cell rows cols row col))
  (case direction
    (:n (values (wrap (1- row) rows) col))
    (:s (values (wrap (1+ row) rows) col))
    (:e (values row (wrap (1+ col) cols)))
    (:w (values row (wrap (1- col) cols)))
    (:c (values row col))))

;;*****************************************************************************************************
;;related to visualization

(defun gen-colors (no-of-players)
  (let ((colors nil))
    (loop repeat no-of-players
          do
          (push (sdl::color :r (random 256)
                            :g (random 256)
                            :b (random 256))
                colors))
    (push sdl::*yellow* colors)
    (push (sdl::color :r 10 :g 60 :b 80) colors) ;    (push sdl::*blue* colors)
    (push nil colors)
    colors))

(defun show-board (rows cols colors board)
  (loop for row below rows do
       (loop for col below cols
	  for cell = (aref board (2-to-1 rows cols row col))
	  for (id index) = (multiple-value-list (decode-byte cell))
	  when (/= id 0)
	  do (vis:put-glyph col row (nth id colors)))))
	  ;; when (= index 1)
	  ;; do (vis:put-glyph col row sdl::*white* ))))

(defun per-user-plot ()
  (let ((cols (cols *board*)))
    (print "in per-user-plot")
    (print cols)
    (loop for player in (players *board*) do
	 (loop for (row col color) in (list-to-screen player) do
;	      (vis:put-glyph (+ col (* cols *scale*)) row color)
	      (vis:put-glyph col row color)

))))

(defun diplay-player-stats (rows cols scale players)
  (declare (ignore rows))
  (loop for player in players
        with s-x-c = 0
        when player do
        (vis:put-text (* scale cols)
                      (* 2 scale s-x-c)
                      (with-output-to-string (s) (format s "p-~d:" (player-id player)))
                      :val (len player)
                      :j :right)
        (incf s-x-c 1)))
;;*****************************************************************************************************
