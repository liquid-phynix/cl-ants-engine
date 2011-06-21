(defclass c1 ()
  ((myvar :accessor myvar :initform nil)))

(defmethod ((theclass c1) &optional val)
  (print (myvar theclass)))
