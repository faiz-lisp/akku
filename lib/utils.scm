;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0+

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
    iota last span! append-map filter-map map-in-order delete-duplicates
    string-prefix? string-suffix?
    string-index string-split
    mkdir/recursive split-path path-join
    read-shebang)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (only (akku lib compat) file-directory? mkdir))

(define (print . x*)
  (for-each (lambda (x)
              (display x (current-error-port)))
            x*)
  (newline (current-error-port)))

(define (append-map f ls)
  (apply append (map f ls)))

(define (filter-map f xs)
  (filter (lambda (x) x) (map f xs)))

(define (map-in-order f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map-in-order f (cdr xs)))))

(define (iota n)
  (unless (>= n 0)
    (error 'iota "Argument must be non-negative" n))
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (- n 1) acc)))))

(define (last x*)
  (if (and (pair? x*) (null? (cdr x*)))
      (car x*)
      (last (cdr x*))))

(define (span! pred? head)
  (let lp ((tail head) (prev #f))
    (cond ((null? tail)
           (values head tail))
          ((pred? (car tail))
           (lp (cdr tail) tail))
          ((not prev)
           (values '() head))
          (else
           (set-cdr! prev '())
           (values head tail)))))

(define (delete-duplicates xs =?)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete-duplicates (remp (lambda (y) (=? (car xs) y))
                                     xs)
                               =?))))

(define (string-prefix? s1 s2)
  ;; is s1 a prefix of s2?
  (define start1 0)
  (define start2 0)
  (define end1 (string-length s1))
  (define end2 (string-length s2))
  (let lp ((start1 start1)
           (start2 start2))
    (cond ((= start1 end1) #t)
          ((= start2 end2) #f)
          ((not (char=? (string-ref s1 start1)
                        (string-ref s2 start2)))
           #f)
          (else
           (lp (+ start1 1)
               (+ start2 1))))))

(define (string-suffix? s1 s2)
  ;; is s1 a suffix of s2?
  (let ((l1 (string-length s1))
        (l2 (string-length s2)))
    (cond ((> l1 l2)
           #f)
          (else
           (let lp ((i 0))
             (cond ((= i l1) #t)
                   ((not (char=? (string-ref s1 i)
                                 (string-ref s2 (+ (- l2 l1) i))))
                    #f)
                   (else
                    (lp (+ i 1)))))))))

(define string-index
  (case-lambda
    ((s c)
     (string-index s c 0))
    ((s c start)
     (string-index s c start (string-length s)))
    ((s c start end)
     (let lp ((i start))
       (and (not (= i end))
            (let ((c* (string-ref s i)))
              (if (char=? c c*)
                  i
                  (lp (+ i 1)))))))))

(define string-split
  (case-lambda
    ((str c max start end)
     (cond ((zero? max)
            (list (substring str start end)))
           ((string-index str c start end) =>
            (lambda (i)
              (cons (substring str start i)
                    (string-split str c (- max 1) (+ i 1) end))))
           (else
            (list (substring str start end)))))
    ((str c max start)
     (string-split str c max start (string-length str)))
    ((str c max)
     (string-split str c max 0 (string-length str)))
    ((str c)
     (string-split str c -1 0 (string-length str)))))

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
             (dir (car component*)))
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
