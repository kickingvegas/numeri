;;; numeri.el --- Roman Numeral Conversion Library   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/numeri
;; Keywords: tools
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

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

;; numeri

;; Numeri is an Emacs Lisp package to support the conversion of Hindu-Arabic
;; numbers to Roman and vice-versa. It is built off utility functions provided
;; by the Org (ox) and reStructuredText (rst) packages. Only integer numbers are
;; supported.

;; INSTALL

;; Install `numeri' from MELPA or MELPA stable. The commands in the package are
;; auto-loaded so no changes are required in your initialization file.

;; If installed from source, you will need to require the package `numeri' in
;; your Emacs initialization file.

;;   (require 'numeri)

;; USAGE

;; There are two commands provided:

;; - `numeri-arabic-to-roman'

;;   This command will accept either an Arabic integer number selected as a
;;   region or input via mini-buffer prompt and convert it to its Roman
;;   equivalent. The result is copied into the kill-ring.

;; - `numeri-roman-to-arabic'

;;   This command will accept either a Roman integer number selected as a region
;;   or input via mini-buffer prompt and convert it to its Arabic equivalent.
;;   The result is copied into the kill-ring.


;;; Code:
(require 'rst)
(require 'ox)

;;;###autoload (autoload 'numeri-arabic-to-roman "numeri" nil t)
(defun numeri-arabic-to-roman (arg1 &optional arg2)
  "Convert Arabic number to Roman via region (ARG1, ARG2) or prompt (ARG1).

This command will accept either an Arabic integer number selected as a
region or input via mini-buffer prompt and convert it to its Roman
equivalent. The result is copied into the `kill-ring'."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (read-number "Arabic number: "))))

  (let ((result nil))
    (if (region-active-p)
        (let ((n (string-to-number (buffer-substring-no-properties arg1 arg2))))
          (setq result (org-export-number-to-roman n)))
      (setq result (org-export-number-to-roman arg1)))

    (message "%s" result)
    (kill-new result)
    result))


;;;###autoload (autoload 'numeri-roman-to-arabic "numeri" nil t)
(defun numeri-roman-to-arabic (arg1 &optional arg2)
  "Convert Roman number to Arabic via region (ARG1, ARG2) or prompt (ARG1).

This command will accept either a Roman integer number selected as a
region or input via mini-buffer prompt and convert it to its Arabic
equivalent. The result is copied into the `kill-ring'."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (read-string "Roman number: "))))

  (let ((result nil))
    (if (region-active-p)
        (setq result (rst-roman-to-arabic (buffer-substring-no-properties arg1 arg2)))
      (setq result (rst-roman-to-arabic arg1)))

    (message "%d" result)
    (kill-new (number-to-string result))
    result))

(defalias 'numeri-number-to-roman 'numeri-arabic-to-roman)

(provide 'numeri)
;;; numeri.el ends here
