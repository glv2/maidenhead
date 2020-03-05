#|
Convert geographic coordinates between Latitude/Longitude and Maidenhead
locator system.

Copyright 2020 Guillaume LE VAILLANT

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

(defpackage :maidenhead-tests
  (:use :common-lisp :fiveam :maidenhead))

(in-package :maidenhead-tests)


(def-suite maidenhead-tests)

(in-suite maidenhead-tests)

(defconstant +max-error-rate+ 0.0001)

(defun close-enough (x y)
  (<= (- 1 +max-error-rate+) (abs (/ x y)) (+ 1 +max-error-rate+)))

(defparameter *maidenhead-data*
  '(((36.165926 -86.723285) "EM66pd39")
    ((-33.014673 116.230695) "OF86cx76")
    ((-55.315349 -68.794971) "FD54oq44")
    ((35.205535 136.56579) "PM85ge79")))

(test lat/lon->maidenhead
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let* ((locator (second data))
                   (maidenhead (lat/lon->maidenhead latitude longitude)))
              (is (string= locator maidenhead)))))
        *maidenhead-data*))

(test maidenhead->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((locator (second data)))
              (destructuring-bind (lat lon) (maidenhead->lat/lon locator)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *maidenhead-data*))
