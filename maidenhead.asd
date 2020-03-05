#|
Convert geographic coordinates between Latitude/Longitude and Maidenhead
locator system.

Copyright 2020 Guillaume Le Vaillant

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

(asdf:defsystem "maidenhead"
  :name "maidenhead"
  :description "Convert coordinates between Latitude/Longitude and Maidenhead."
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :in-order-to ((test-op (test-op "maidenhead/tests")))
  :components ((:file "maidenhead")))

(asdf:defsystem "maidenhead/tests"
  :name "maidenhead/tests"
  :description "Unit tests for maidenhead"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("fiveam" "maidenhead")
  :in-order-to ((test-op (load-op "maidenhead/tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'maidenhead-tests :maidenhead-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "maidenhead-tests")))
