;;; alg.el --- Algorithms, algebra  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Version: 0.2.1
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

(defun alg-merge-sorted (sequence1 sequence2 &optional compare)
  "Merge sorted sequences SEQUENCE1 and SEQUENCE2 with comparison COMPARE.

\(O (+ (length sequence1) (length sequence2)))"
  (unless compare (setq compare #'<))
  (let (result)
    (while (and sequence1 sequence2)
      (push
       (if (funcall compare (car sequence1) (car sequence2))
           (pop sequence1)
         (pop sequence2))
       result))
    (nconc (nreverse result) (or sequence1 sequence2))))

(defun alg-eratosthenes-sieve (n)
  "Return vector of prime numbers less than or equal to N.

\(O (* n (log (log n))))"
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

(defun alg-atkin-sieve (n)
  "Return vector of prime numers less than or equal to N.

\(O (/ n (log (log n))))"
  (let* ((s1 '(1 13 17 29 37 41 49 53))
         (s2 '(7 19 31 43))
         (s3 '(11 23 47 59))
         (s (alg-merge-sorted (alg-merge-sorted s1 s2) s3))
         (result (make-hash-table)))
    (dolist (i (number-sequence 0 (/ n 60)))
      (dolist (j s)
        (puthash (+ (* i 60) j) nil result)))

    (let (k
          (x 1)
          y)
      (while (<= (* 4 x x) n)
        (setq y 1)
        (while (<= (setq k (+ (* 4 x x) (* y y))) n)
          (when (memq (mod k 60) s1)
            (puthash k (not (gethash k result)) result))
          (cl-incf y 2))
        (cl-incf x)))

    (let (k
          (x 1)
          y)
      (while (<= (* 3 x x) n)
        (setq y 2)
        (while (<= (setq k (+ (* 3 x x) (* y y))) n)
          (when (memq (mod k 60) s2)
            (puthash k (not (gethash k result)) result))
          (cl-incf y 2))
       (cl-incf x 2)))

    (let (k
          (x 2)
          y)
      (while (<= (* 2 x x) n)
        (setq y (1- x))
        (while (and (<= (setq k (- (* 3 x x) (* y y))) n)
                    (< 0 y))
          (when (memq (mod k 60) s3)
            (puthash k (not (gethash k result)) result))
          (cl-decf y 2))
        (cl-incf x)))

    (dotimes (i (1+ (/ n 60)))
      (dolist (j s)
        (let ((k (+ (* i 60) j)))
          (when (and (<= 7 k)
                     (gethash k result))
            (dolist (i2 (number-sequence 0 (/  n (* k k) 60)))
              (dolist (j2 s)
                (puthash (* k k (+ (* i2 60) j2)) nil result)))))))

    (let (res k)
     (dolist (i (number-sequence 0 (/ n 60)))
      (dolist (j s)
        (when (gethash (setq k (+ (* i 60) j)) result)
          (push k res))))
     (vconcat
      (pcase n
        ((pred (<= 5)) '(2 3 5))
        ((pred (<= 3)) '(2 3))
        ((pred (<= 2)) '(2)))
      (nreverse res)))))

(provide 'alg)
;;; alg.el ends here
