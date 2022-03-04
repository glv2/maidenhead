#|
Convert geographic coordinates between Latitude/Longitude and Maidenhead
locator system.

Copyright 2020-2022 Guillaume Le Vaillant

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage :maidenhead
  (:use :common-lisp)
  (:export #:lat/lon->maidenhead
           #:maidenhead->lat/lon))

(in-package :maidenhead)


(defun lat/lon->maidenhead (latitude longitude)
  "Return the Maidenhead locator for the given LATITUDE and LONGITUDE."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (let ((field-letters (load-time-value "ABCDEFGHIJKLMNOPQR" t))
        (subsquare-letters (load-time-value "abcdefghijklmnopqrstuvwx" t))
        (latitude (+ latitude 90.0d0))
        (longitude (/ (+ longitude 180.0d0) 2)))
    (multiple-value-bind (field-x rx) (floor longitude 10)
      (multiple-value-bind (field-y ry) (floor latitude 10)
        (multiple-value-bind (square-x rx) (floor rx)
          (multiple-value-bind (square-y ry) (floor ry)
            (multiple-value-bind (subsquare-x rx) (floor (* rx 24))
              (multiple-value-bind (subsquare-y ry) (floor (* ry 24))
                (let ((extsquare-x (floor (* rx 10)))
                      (extsquare-y (floor (* ry 10))))
                  (format nil "~c~c~d~d~c~c~d~d"
                          (char field-letters field-x)
                          (char field-letters field-y)
                          square-x
                          square-y
                          (char subsquare-letters subsquare-x)
                          (char subsquare-letters subsquare-y)
                          extsquare-x
                          extsquare-y))))))))))

(defun maidenhead->lat/lon (locator &optional center-p)
  "Return the latitude and longitude for the southwest corner of the given
Maidenhead LOCATOR square, or the center of the square if CENTER-P is not NIL."
  (check-type locator string)
  (let* ((field-letters (load-time-value "ABCDEFGHIJKLMNOPQR" t))
         (subsquare-letters (load-time-value "abcdefghijklmnopqrstuvwx" t))
         (locator (if center-p
                      (case (length locator)
                        ((8) locator)
                        ((6) (concatenate 'string locator "55"))
                        ((4) (concatenate 'string locator "mm55"))
                        ((2) (concatenate 'string locator "55mm55"))
                        (t (error "Invalid locator: ~a" locator)))
                      (case (length locator)
                        ((8) locator)
                        ((6) (concatenate 'string locator "00"))
                        ((4) (concatenate 'string locator "aa00"))
                        ((2) (concatenate 'string locator "00aa00"))
                        (t (error "Invalid locator: ~a" locator)))))
         (field-x (position (char locator 0) field-letters
                            :test #'char-equal))
         (field-y (position (char locator 1) field-letters
                            :test #'char-equal))
         (square-x (parse-integer (string (char locator 2))))
         (square-y (parse-integer (string (char locator 3))))
         (subsquare-x (position (char locator 4) subsquare-letters
                                :test #'char-equal))
         (subsquare-y (position (char locator 5) subsquare-letters
                                :test #'char-equal))
         (extsquare-x (parse-integer (string (char locator 6))))
         (extsquare-y (parse-integer (string (char locator 7)))))
    (if (every #'integerp (list field-x field-y
                                square-x square-y
                                subsquare-x subsquare-y
                                extsquare-x extsquare-y))
        (let ((longitude (- (* 2 (+ (* field-x 10) square-x
                                    (/ subsquare-x 24) (/ extsquare-x 240)))
                            180.0d0))
              (latitude (- (+ (* field-y 10) square-y
                              (/ subsquare-y 24) (/ extsquare-y 240))
                           90.0d0)))
          (if center-p
              (list (+ latitude 1/480) (+ longitude 1/240))
              (list latitude longitude)))
        (error "Invalid locator: ~a" locator))))
