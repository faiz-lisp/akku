;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;; Definition of the lockfile format.

(library (akku format lockfile)
  (export
    lockfile-filename
    projects)
  (import
    (rnrs)
    (only (xitomatl common) pretty-print))

(define lockfile-filename "Akku.lock")

(define-syntax projects
  (lambda (x)
    (define (unique-names? name*)
      (or (null? name*)
          (and (not (memp (lambda (name) (string-ci=? name (car name*))) (cdr name*)))
               (unique-names? (cdr name*)))))

    (syntax-case x (name)
      ((_ ((name name*) attr* ...) ...)
       (unique-names? (syntax->datum #'(name* ...)))
       #'(pretty-print `(projects ,(check-project ((name name*) attr* ...))
                                  ...))))))

(define-syntax check-project
  (lambda (x)
    (syntax-case x (name)
      ((_ ((name project-name)
           attr* ...))
       #'`((name project-name) ,(check-attr attr*) ...)))))

(define-syntax check-attr
  (lambda (x)
    (syntax-case x (location git directory tag revision)
      ((_ (location (git url)))
       (string? (syntax->datum #'url))
       #''(location (git url)))
      ((_ (location (directory path)))
       (string? (syntax->datum #'path))
       #''(location (directory path)))
      ((_ (tag tag-name))
       (string? (syntax->datum #'tag-name))
       #''(tag tag-name))
      ((_ (revision commit-id))
       (string? (syntax->datum #'commit-id))
       #''(revision commit-id))))))
