;;; alg-test.el --- Test for alg

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

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

;; Test for alg

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'alg)

(defvar alg-test-prime-numbers-less-than-1000
  (alg-eratosthenes-sieve 1000)
  [
   2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
   101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
   211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293
   307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397
   401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499
   503 509 521 523 541 547 557 563 569 571 577 587 593 599
   601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691
   701 709 719 727 733 739 743 751 757 761 769 773 787 797
   809 811 821 823 827 829 839 853 857 859 863 877 881 883 887
   907 911 919 929 937 941 947 953 967 971 977 983 991 997
   ])

(ert-deftest alg-merge-sorted ()
  (should
   (equal (alg-merge-sorted '(1 3 5 7) '(2 4 6 8))
          '(1 2 3 4 5 6 7 8))))

(ert-deftest alg-eratosthenes-sieve ()
  (should (equal (alg-eratosthenes-sieve 1) []))
  (should (equal (alg-eratosthenes-sieve 2) [2]))
  (should (equal (alg-eratosthenes-sieve 3) [2 3]))
  (should (equal (alg-eratosthenes-sieve 4) [2 3]))
  (should (equal (alg-eratosthenes-sieve 5) [2 3 5]))
  (should (equal (alg-eratosthenes-sieve 6) [2 3 5]))
  (should (equal (alg-eratosthenes-sieve 7) [2 3 5 7]))
  (should (equal (alg-eratosthenes-sieve 1000) alg-test-prime-numbers-less-than-1000)))

(ert-deftest alg-atkin-sieve ()
  (should (equal (alg-atkin-sieve 1) []))
  (should (equal (alg-atkin-sieve 2) [2]))
  (should (equal (alg-atkin-sieve 3) [2 3]))
  (should (equal (alg-atkin-sieve 4) [2 3]))
  (should (equal (alg-atkin-sieve 5) [2 3 5]))
  (should (equal (alg-atkin-sieve 6) [2 3 5]))
  (should (equal (alg-atkin-sieve 7) [2 3 5 7]))
  (should (equal (alg-atkin-sieve 1000) alg-test-prime-numbers-less-than-1000)))


(provide 'alg-test)
;;; alg-test.el ends here
