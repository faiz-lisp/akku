;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Common utilities that should really be imported from elsewhere.

(library (akku lib utils)
  (export
    print
    append-map filter-map map-in-order delete-duplicates
    string-prefix? string-suffix? string-index
    string-split
    mkdir/recursive split-path path-join
    read-shebang)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (only (srfi :1 lists) append-map filter-map map-in-order delete-duplicates)
    (only (srfi :13 strings) string-prefix? string-suffix? string-index)
    (only (industria strings) string-split)
    (only (akku lib compat) file-directory? mkdir))

(define (print . x*)
  (for-each (lambda (x)
              (display x (current-error-port)))
            x*)
  (newline (current-error-port)))

;; Split directory name and filename components.
(define (split-path filename)
  (let-values (((p extract) (open-string-output-port)))
    (let lp ((part* (string-split filename #\/)))
      (cond ((null? (cdr part*))
             (cons (extract) (car part*)))
            (else
             (put-string p (car part*))
             (unless (null? (cddr part*))
               (put-char p #\/))
             (lp (cdr part*)))))))

;; Join two path components.
(define (path-join x y)
  (cond ((string-suffix? "/" x)
         (string-append x y))
        ((string=? x "")
         y)
        ((string-prefix? "/" y)
         y)
        (else
         (string-append x "/" y))))

(define (mkdir/recursive path)
  (let ((component* (string-split path #\/)))
    (let lp ((component* (cdr component*))
             (dir (if (string-prefix? "/" path)
                      "/"
                      (car component*))))
      (unless (file-directory? dir)
        (mkdir dir))
      (unless (null? component*)
        (lp (cdr component*) (path-join dir (car component*)))))))

(define (read-shebang port)
  (let ((start (port-position port)))
    (let ((line1 (get-line port)))
      (cond ((or (string-prefix? "#! " line1)
                 (string-prefix? "#!/" line1))
             line1)
            (else
             (set-port-position! port start)
             #f))))))
