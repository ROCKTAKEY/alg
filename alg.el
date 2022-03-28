;;; alg.el --- Algorithms, algebra  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.7"))
;; URL: https://github.com/ROCKTAKEY/alg

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

;; Algebra features

;;; Code:

(require 'cl-lib)

(defgroup alg ()
  "Algorithms, algebra."
  :group 'tools
  :prefix "alg-"
  :link '(url-link "https://github.com/ROCKTAKEY/alg"))

(defun alg-eratosthenes-sieve (n)
  "Return vector of prime numbers less than or equal to N.

(O(* n (log (log n))))"
  (let ((numbers (vconcat (number-sequence 0 n)))
        prime-numbers
        (i 0))
    (aset numbers 0 nil)
    (aset numbers 1 nil)

    (while (<= (* i i) n)
      (when (aref numbers i)
        (push i prime-numbers)
        (dolist (j (number-sequence 0 n i))
          (aset numbers j nil)))
      (cl-incf i))

    (vconcat (nreverse prime-numbers)
             (cl-remove-if-not #'identity numbers))))

(provide 'alg)
;;; alg.el ends here
