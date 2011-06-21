; vim: set autoindent:

(in-package :engine)

(defclass player-sc ()
  ((player-ants-vec     :accessor player-ants-vec   :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (enemy-ants-vec      :accessor enemy-ants-vec    :initform (make-array 0 :fill-pointer 0 :adjustable t)) 
   (player-ants-ht      :accessor player-ants-ht    :initform (make-hash-table))
   (enemy-ants-ht       :accessor enemy-ants-ht     :initform (make-hash-table))
   (water-ht            :accessor water             :initform (make-hash-table))
   ;   (food-ht             :accessor food              :initform (make-hash-table))
   (food-vec            :accessor food              :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (excluded-coords     :accessor excluded          :initform (make-hash-table))))

(defmethod ant-count ((c player-sc))
  (length (player-ants-vec c)))

(defmethod player-start-turn :around ((c player-sc))
  (setf (player-ants-vec c)  (make-array 0 :fill-pointer 0 :adjustable t)
        (enemy-ants-vec c)   (make-array 0 :fill-pointer 0 :adjustable t)
        (player-ants-ht c)   (make-hash-table)
        (enemy-ants-ht c)    (make-hash-table)
        ;        (food c)             (make-hash-table) 
        (food c)             (make-array 0 :fill-pointer 0 :adjustable t)
        (excluded c)         (make-hash-table))
  (call-next-method))

(defmethod player-process-ant ((c player-sc) row col ant-id)
  (if (= ant-id 0)
    (player-process-own-ant c row col)
    (player-process-enemy-ant c row col ant-id)))

(defmethod player-process-own-ant ((c player-sc) row col)
  (extend-ht row col (player-ants-ht c))
  (vector-push-extend (make-struct-r-c :row row :col col) (player-ants-vec c)))

(defmethod player-process-enemy-ant ((c player-sc) row col ant-id)
  (extend-ht row col (enemy-ants-ht c))
  (vector-push-extend (make-struct-r-c :row row :col col) (enemy-ants-vec c)))

(defmethod player-process-water ((c player-sc) row col)
  (extend-ht row col (water c)))

(defmethod player-process-food ((c player-sc) row col)
  (vector-push-extend (make-struct-r-c :row row :col col) (food c)))
;  (extend-ht row col (food c)))

(defmethod add-to-excluded ((c player-sc) row col)
  (extend-ht row col (excluded c)))
;ne lepjunk oda se, ahol food van
;mert a buta engine megol!!!!!!!!!!!!!
(defmethod forbidden-directions ((c player-sc) row col)
  (let ((possible-coordinates `((,(wrap-coords (1+ row) col) :south)
                                (,(wrap-coords (1- row) col) :north)
                                (,(wrap-coords row (1+ col)) :east)
                                (,(wrap-coords row (1- col)) :west)))
        free-directions)
    (setf free-directions (loop for triple in possible-coordinates
                                when (and (not (part-of-ht  (first (first triple)) (second (first triple)) (water c)))
                                          (not (part-of-ht  (first (first triple)) (second (first triple)) (excluded c)))
                                          (not (part-of-src-vec (first (first triple)) (second (first triple)) (food c))))
                                ;(not (part-of-ht  (first (first triple)) (second (first triple)) (food c))))
                                collect (second triple)))
    (filter '(:north :south :east :west) free-directions)))

(defmethod mean-position ((c player-sc))
  ;  (print-stderr (player-ants-vec c))
  (loop for ant across (player-ants-vec c)
        with row-cum = 0
        with col-cum = 0
        do
        (setf row-cum (+ row-cum (struct-r-c-row ant))
              col-cum (+ col-cum (struct-r-c-row ant)))
        finally (return (values (/ row-cum (ant-count c)) (/ col-cum (ant-count c))))))
