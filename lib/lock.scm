;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
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

;; Lock file creation.

(library (akku lib lock)
  (export
    lock-dependencies)
  (import
    (rnrs (6))
    (only (srfi :1 lists) iota)
    (spells logging)
    (wak fmt)
    (xitomatl AS-match)
    (akku lib solver)
    (akku lib solver choice)
    (akku lib solver dummy-db)
    (only (akku lib solver internals) make-universe)
    (akku lib solver logging)
    (prefix (akku lib solver universe) universe-)

    (only (akku lib compat) pretty-print))

(define (simple-log-formatter entry)
  (let ((port (current-error-port))
        (obj (log-entry-object entry)))
    (if (procedure? obj)
        (obj port)
        (display obj port))
    (newline port)))

(define (assq-ref alist key default)
  (cond ((assq key alist) => cdr)
        (else default)))

(define (get-depends db manifest-filename)
  (call-with-input-file manifest-filename
    (lambda (p)
      (read p)
      (let ((manifest (read p)))
        (match manifest
          (('akku-package id prop* ...)
           (let ((depends (cond ((assq 'depends prop*) => cdr)
                                (else '()))))
             (let ((version-scores
                    (map (match-lambda
                          ((pkg-name constraint)
                           ;; XXX: Use the version constraint to pick
                           ;; versions and better scores.
                           (cons (dummy-db-version-ref db pkg-name 0) 10000)))
                         depends))
                   (initial-choices
                    (fold-left (lambda (choices x)
                                 (match x
                                   ((pkg-name constraint)
                                    (choice-set-insert-or-narrow
                                     choices
                                     ;; XXX: Use the constraint to pick a version...?
                                     (make-install-choice (dummy-db-version-ref db pkg-name 0)
                                                          #f)))))
                               (make-choice-set)
                               depends)))
               (values version-scores initial-choices)))))))))

;; Takes two choice-sets containing chosen packages and returns list
;; of projects for a lockfile.
(define (choice-set->project-list index-filename initial-choices choices-in-solution)
  (define (choice->project choice)
    (let* ((chosen-tag (universe-version-tag (choice-version choice)))
           (pkg (universe-version-package (choice-version choice)))
           (name (universe-package-name pkg))
           (REQUESTED-VERSION (choice-set-version-of initial-choices pkg))
           (current-version (universe-package-current-version pkg))
           (current-tag (universe-version-tag current-version)))
      (cond ((and REQUESTED-VERSION
                  (universe-version-tag REQUESTED-VERSION)
                  (not chosen-tag))
             #f)
            ((not chosen-tag)
             `((name ,name) (no project chosen!)))
            ((equal? name "SELF") 'filter-me)
            (else
             (let ((project (index-get-package-version index-filename name chosen-tag)))
               `((name ,name)
                 ,@(cdr (assq 'lock project))
                 ))))))

  (choice-set-fold (lambda (choice acc)
                     (cons (choice->project choice) acc))
                   '()
                   (choice-set-union initial-choices choices-in-solution)))

(define (index-get-package-version index-filename package-name version-tag)
  (call-with-input-file index-filename
    (lambda (p)
      (let lp ()
        (match (read p)
          ((? eof-object?) #f)
          (('package ('name (? (lambda (name) (equal? package-name name))))
                     ('versions versions ...))
           (list-ref versions version-tag))
          (else (lp)))))))

(define (lock-dependencies manifest-filename lockfile-filename index-filename)
  (set-logger-properties! root-logger
                          `((threshold trace)
                            (handlers ,simple-log-formatter)))
  (let ((db (make-dummy-db))
        (dev-mode? #t))
    (call-with-input-file index-filename
      (lambda (p)
        (let lp ()
          (match (read p)
            ((? eof-object?) #f)
            (('package ('name name)
                       ('versions versions ...))
             (dummy-db-add-package! db name (cons #f (iota (length versions))) #f)
             (lp))))))

    (call-with-input-file index-filename
      (lambda (p)
        (let lp ()
          (match (read p)
            ((? eof-object?) #f)
            (('package ('name name)
                       ('versions versions ...))
             (for-each (lambda (version-idx version-spec)
                         (let ((depends (assq-ref version-spec 'depends '()))
                               (depends/dev (assq-ref version-spec 'depends/dev '()))
                               (conflicts (assq-ref version-spec 'conflicts '())))
                           (define (process-list lst conflict?)
                             (unless (null? (cdr lst))
                               (dummy-db-add-dependency! db name version-idx conflict?
                                                         (map (lambda (x)
                                                                (cons (car lst) x))
                                                              (cdr lst)))))
                           (for-each (lambda (dep) (process-list dep #f))
                                     (append depends (if dev-mode? depends/dev '())))
                           (for-each (lambda (dep) (process-list dep #t))
                                     conflicts)))
                       (iota (length versions)) versions)
             (lp))))))

    (let-values (((version-scores initial-choices) (get-depends db manifest-filename)))
      (let* ((universe (dummy-db->universe db))
             (solver (make-solver universe
                                  `((version-scores . ,version-scores)
                                    (initial-choices . ,initial-choices)))))
        (fmt #t (dsp-universe universe))
        (newline)
        (let lp ()
          (let ((s0 (find-next-solution! solver 10000)))
            (unless (not s0)
              (newline)
              (newline)
              (for-each
               (lambda (c)
                 (let* ((chosen-tag (universe-version-tag (choice-version c)))
                        (pkg (universe-version-package (choice-version c)))
                        (name (universe-package-name pkg))
                        (REQUESTED-VERSION (choice-set-version-of initial-choices pkg))
                        (current-version (universe-package-current-version pkg))
                        (current-tag (universe-version-tag current-version)))
                   (when (and REQUESTED-VERSION
                              (universe-version-tag REQUESTED-VERSION)
                              (not chosen-tag))
                     (display "not acceptable\n"))
                   (write (list 'name name 'chosen chosen-tag 'current current-tag))
                   (newline)))
               (choice-set->list (choice-set-union
                                  initial-choices
                                  (solution-choices s0))))

              (let ((projects (choice-set->project-list index-filename
                                                        initial-choices
                                                        (solution-choices s0))))
                (display "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-\n")
                (pretty-print `(import (akku format lockfile)))
                (pretty-print `(projects ,@projects)))
              (lp))))))))

)
