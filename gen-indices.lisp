; vim: set autoindent:

(in-package :engine)

(defun gen-cat-1-indices (rows cols row col)
  (declare (type cell rows cols row col))
  (let ((result (make-array 4 :initial-element 0 :element-type 'cell)))
    (setf 
      (aref result 0) (+ (wrap (+ -1 col) cols) (* cols (wrap row rows)))
      (aref result 1) (+ (wrap col cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 2) (+ (wrap col cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 3) (+ (wrap (+ 1 col) cols) (* cols (wrap row rows))))
    result))

(defun gen-cat-2-indices (rows cols row col)
  (declare (type cell rows cols row col))
  (let ((result (make-array 8 :initial-element 0 :element-type 'cell)))
    (setf
      (aref result 0) (+ (wrap (+ -2 col) cols) (* cols (wrap row rows)))
      (aref result 1) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 2) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 3) (+ (wrap col cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 4) (+ (wrap col cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 5) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 6) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 7) (+ (wrap (+ 2 col) cols) (* cols (wrap row rows))))
    result))

(defun gen-cat-3-indices (rows cols row col)
  (declare (type cell rows cols row col))
  (let ((result (make-array 164 :initial-element 0 :element-type 'cell)))
    (setf
      (aref result 0) (+ (wrap (+ -7 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 1) (+ (wrap (+ -7 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 2) (+ (wrap (+ -7 col) cols) (* cols (wrap row rows)))
      (aref result 3) (+ (wrap (+ -7 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 4) (+ (wrap (+ -7 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 5) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 6) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 7) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 8) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 9) (+ (wrap (+ -6 col) cols) (* cols (wrap row rows)))
      (aref result 10) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 11) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 12) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 13) (+ (wrap (+ -6 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 14) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 15) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 16) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 17) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 18) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 19) (+ (wrap (+ -5 col) cols) (* cols (wrap row rows)))
      (aref result 20) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 21) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 22) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 23) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 24) (+ (wrap (+ -5 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 25) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 26) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 27) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 28) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 29) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 30) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 31) (+ (wrap (+ -4 col) cols) (* cols (wrap row rows)))
      (aref result 32) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 33) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 34) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 35) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 36) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 37) (+ (wrap (+ -4 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 38) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 39) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 40) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 41) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 42) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 43) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 44) (+ (wrap (+ -3 col) cols) (* cols (wrap row rows)))
      (aref result 45) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 46) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 47) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 48) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 49) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 50) (+ (wrap (+ -3 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 51) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -7 row) rows)))
      (aref result 52) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 53) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 54) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 55) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 56) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 57) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 58) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 59) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 60) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 61) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 62) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 63) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 64) (+ (wrap (+ -2 col) cols) (* cols (wrap (+ 7 row) rows)))
      (aref result 65) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -7 row) rows)))
      (aref result 66) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 67) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 68) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 69) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 70) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 71) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 72) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 73) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 74) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 75) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 76) (+ (wrap (+ -1 col) cols) (* cols (wrap (+ 7 row) rows)))
      (aref result 77) (+ (wrap col cols) (* cols (wrap (+ -7 row) rows)))
      (aref result 78) (+ (wrap col cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 79) (+ (wrap col cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 80) (+ (wrap col cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 81) (+ (wrap col cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 82) (+ (wrap col cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 83) (+ (wrap col cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 84) (+ (wrap col cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 85) (+ (wrap col cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 86) (+ (wrap col cols) (* cols (wrap (+ 7 row) rows)))
      (aref result 87) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -7 row) rows)))
      (aref result 88) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 89) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 90) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 91) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 92) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 93) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 94) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 95) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 96) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 97) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 98) (+ (wrap (+ 1 col) cols) (* cols (wrap (+ 7 row) rows)))
      (aref result 99) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -7 row) rows)))
      (aref result 100) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 101) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 102) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 103) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 104) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 105) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 106) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 107) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 108) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 109) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 110) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 111) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 112) (+ (wrap (+ 2 col) cols) (* cols (wrap (+ 7 row) rows)))
      (aref result 113) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 114) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 115) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 116) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 117) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 118) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 119) (+ (wrap (+ 3 col) cols) (* cols (wrap row rows)))
      (aref result 120) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 121) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 122) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 123) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 124) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 125) (+ (wrap (+ 3 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 126) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -6 row) rows)))
      (aref result 127) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 128) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 129) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 130) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 131) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 132) (+ (wrap (+ 4 col) cols) (* cols (wrap row rows)))
      (aref result 133) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 134) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 135) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 136) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 137) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 138) (+ (wrap (+ 4 col) cols) (* cols (wrap (+ 6 row) rows)))
      (aref result 139) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ -5 row) rows)))
      (aref result 140) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 141) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 142) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 143) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 144) (+ (wrap (+ 5 col) cols) (* cols (wrap row rows)))
      (aref result 145) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 146) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 147) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 148) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 149) (+ (wrap (+ 5 col) cols) (* cols (wrap (+ 5 row) rows)))
      (aref result 150) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ -4 row) rows)))
      (aref result 151) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ -3 row) rows)))
      (aref result 152) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 153) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 154) (+ (wrap (+ 6 col) cols) (* cols (wrap row rows)))
      (aref result 155) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 156) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ 2 row) rows)))
      (aref result 157) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ 3 row) rows)))
      (aref result 158) (+ (wrap (+ 6 col) cols) (* cols (wrap (+ 4 row) rows)))
      (aref result 159) (+ (wrap (+ 7 col) cols) (* cols (wrap (+ -2 row) rows)))
      (aref result 160) (+ (wrap (+ 7 col) cols) (* cols (wrap (+ -1 row) rows)))
      (aref result 161) (+ (wrap (+ 7 col) cols) (* cols (wrap row rows)))
      (aref result 162) (+ (wrap (+ 7 col) cols) (* cols (wrap (+ 1 row) rows)))
      (aref result 163) (+ (wrap (+ 7 col) cols) (* cols (wrap (+ 2 row) rows))))
    result))