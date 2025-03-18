;;; test-numeri.el --- Numeri Tests                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi
;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'numeri-test-utils)

(ert-deftest test-numeri-arabic-to-roman ()
  (let ((test-vectors '((1 . "I")
                        (2 . "II")
                        (3 . "III")
                        (4 . "IV")
                        (5 . "V")
                        (6 . "VI")
                        (7 . "VII")
                        (8 . "VIII")
                        (9 . "IX")
                        (10 . "X")
                        (60 . "LX"))))
    (map-do (lambda (input expected)
              (should
               (string-equal (numeri-arabic-to-roman input) expected)))
            test-vectors)))

(ert-deftest test-numeri-roman-to-arabic ()
  (let ((test-vectors '((1 . "I")
                        (2 . "II")
                        (3 . "III")
                        (4 . "IV")
                        (5 . "V")
                        (6 . "VI")
                        (7 . "VII")
                        (8 . "VIII")
                        (9 . "IX")
                        (10 . "X")
                        (60 . "LX"))))
    (map-do (lambda (expected input)
              (should
               (eq (numeri-roman-to-arabic input) expected)))
            test-vectors)))

(ert-deftest test-numeri-random ()
  (dotimes (i 200 "numeri random test complete")
    (let* ((a (+ (random 1000) 1)) ; avoid 0
           (r (numeri-arabic-to-roman a))
           (a2 (numeri-roman-to-arabic r)))
      ;; (print (format "%d: %d %s %d" i a r a2))
      (should (eq a a2)))))

(provide 'test-numeri)
;;; test-numeri.el ends here
