;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
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

;; Lock file creation.

(library (akku lib lock)
  (export
    lock-dependencies
    logger:akku.lock)
  (import
    (rnrs (6))
    (only (srfi :1 lists) iota append-map filter-map)
    (semver versions)
    (semver ranges)
    (spdx parser)
    (spells logging)
    (wak fmt)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (akku lib compat) pretty-print rename-file)
    (akku lib solver)
    (akku lib solver choice)
    (akku lib solver dummy-db)          ;TODO: Make a proper database
    (only (akku lib solver internals) make-universe)
    (only (akku lib solver logging) dsp-universe)
    (only (akku private utils) make-fmt-log logger:akku)
    (prefix (akku lib solver universe) universe-))

(define logger:akku.lock (make-logger logger:akku 'lock))
(define log/info (make-fmt-log logger:akku.lock 'info))
(define log/debug (make-fmt-log logger:akku.lock 'debug))
(define log/trace (make-fmt-log logger:akku.lock 'trace))

(define-record-type package
  (nongenerative)
  (sealed #t)
  (fields name version*))

(define-record-type version
  (nongenerative)
  (sealed #t)
  (fields number semver lock depends depends/dev conflicts))

(define (parse-version version-spec)
  (let ((version-number (car (assq-ref version-spec 'version))))
    (make-version version-number
                  (string->semver version-number)
                  (assq-ref version-spec 'lock)
                  (assq-ref version-spec 'depends '())
                  (assq-ref version-spec 'depends/dev '())
                  (assq-ref version-spec 'conflicts '()))))

(define (read-package-index index-filename manifest-packages)
  (let ((db (make-dummy-db))
        (packages (make-hashtable equal-hash equal?)))
    ;; Add the packages from the manifest.
    (for-each (lambda (pkg)
                (dummy-db-add-package! db (package-name pkg) (list 0) 0)
                (hashtable-set! packages (package-name pkg) pkg))
              manifest-packages)
    ;; Read packages from the index.
    (call-with-input-file index-filename
      (lambda (p)
        (let lp ()
          (match (read p)
            ((? eof-object?) #f)
            (('package ('name name)
                       ('versions version* ...))
             (unless (memp (lambda (pkg) (equal? (package-name pkg) name))
                           manifest-packages)
               ;; XXX: Versions must be semver-sorted in ascending
               ;; order.
               (dummy-db-add-package! db name (cons #f (iota (length version*))) #f)
               (hashtable-set! packages name
                               (make-package name (map parse-version version*))))
             (lp))
            (else (lp))))))             ;allow for future expansion
    (values db packages)))

(define (read-manifest manifest-filename)
  (call-with-input-file manifest-filename
    (lambda (p)
      (let lp ((pkg* '()) (name* '()))
        (match (read p)
          (('akku-package (name version) prop* ...)
           (assert (not (member name name*)))
           (let ((license-expr (car (assq-ref prop* 'license))))
             (or (member license-expr '("NONE" "NOASSERTION"))
                 (parse-license-expression license-expr))) ;TODO: validate
           (let* ((ver (parse-version `((version ,version)
                                        (lock #f)
                                        ,@prop*)))
                  (pkg (make-package name (list ver))))
             (lp (cons pkg pkg*) (cons name name*))))
          ((? eof-object?)
           pkg*)
          (else (lp pkg* name*)))))))   ;allow for future expansion

;; Get scores and choices for the packages in the manifest. These are
;; scored very high and set to already be installed.
(define (scores/choices db manifest-packages)
  (let lp ((manifest-packages manifest-packages)
           (version-scores '())
           (initial-choices (make-choice-set)))
    (cond
      ((null? manifest-packages)
       (values version-scores initial-choices))
      (else
       (let* ((pkg (car manifest-packages))
              (pkg-name (package-name pkg)))
         (lp (cdr manifest-packages)
             (cons (cons (dummy-db-version-ref db pkg-name 0) 10000)
                   version-scores)
             (choice-set-insert-or-narrow
              initial-choices
              (make-install-choice (dummy-db-version-ref db pkg-name 0) 0))))))))

;; Takes two choice-sets containing chosen packages and returns a list
;; of projects for a lockfile.
(define (choice-set->project-list packages manifest-packages
                                  initial-choices choices-in-solution)
  (define (choice->project choice)
    (let* ((chosen-tag (universe-version-tag (choice-version choice)))
           (pkg (universe-version-package (choice-version choice)))
           (name (universe-package-name pkg))
           (requested-version (choice-set-version-of initial-choices pkg))
           (current-tag (universe-version-tag (universe-package-current-version pkg))))
      (log/debug "Project " name " v" chosen-tag " (was v" current-tag ")")
      (cond ((and requested-version
                  (universe-version-tag requested-version)
                  (not chosen-tag))
             ;; A package from the manifest was not chosen.
             #f)
            ((memp (lambda (pkg) (equal? (package-name pkg) name))
                   manifest-packages)
             ;; Don't return a project for packages in the manifest.
             'in-manifest)
            ((not chosen-tag)
             `((name ,name) (no project chosen!)))
            (else
             ;; This goes into the lockfile.
             (let* ((pkg (hashtable-ref packages name #f))
                    (ver (list-ref (package-version* pkg) chosen-tag)))
               (log/info "Locked " name " v" (version-number ver))
               `((name ,name)
                 ,@(version-lock ver)))))))
  (choice-set-fold (lambda (choice acc)
                     (let ((project (choice->project choice)))
                       (if (eq? project 'in-manifest)
                           acc
                           (cons project acc))))
                   '()
                   (choice-set-union initial-choices choices-in-solution)))

(define (dependencies->version-tags packages pkg lst)
  (let lp ((lst lst))
    (match lst
      [('or pkg* ...)
       (append-map lp pkg*)]
      [(name (? string? range))
       ;; TODO: Don't crash when the depended-on package doesn't
       ;; exist. Don't crash when no versions are in the range.
       (let* ((available-version* (package-version*
                                   (hashtable-ref packages name 'FIXME:no-such-pkg)))
              (m (semver-range->matcher range))
              (tag* (filter-map
                     (lambda (tag pkgver)
                       (and (m (version-semver pkgver)) tag))
                     (iota (length available-version*))
                     available-version*)))
         (when (null? tag*)
           (error 'dependencies->version-tags "TODO: No matching versions"
                  (package-name pkg)
                  name
                  (semver-range->string (semver-range-desugar (string->semver-range range)))
                  (map version-number available-version*)))
         ;; To satisfy the dependency, any of these (name . tag) pairs
         ;; can be used.
         (map (lambda (tag) (cons name tag)) tag*))])))

;; Adds dependencies between packages.
(define (add-package-dependencies db packages manifest-packages dev-mode?)
  (define (process-package-version pkg version-idx version)
    (define (process-deps lst conflict?)
      (let ((deps (dependencies->version-tags packages pkg lst)))
        (unless (null? deps)
          (dummy-db-add-dependency! db (package-name pkg) version-idx conflict?
                                    deps))))
    (for-each (lambda (dep) (process-deps dep #f))
              (version-depends version))
    (for-each (lambda (dep) (process-deps dep #t))
              (version-conflicts version))
    (when (and dev-mode? (memq pkg manifest-packages))
      ;; Dev mode: add dev dependencies for packages in the manifest.
      (for-each (lambda (dep) (process-deps dep #f))
                (version-depends/dev version))))
  (let-values (((pkg-names pkgs) (hashtable-entries packages)))
      (vector-for-each
       (lambda (name pkg)
         (for-each (lambda (version-idx version)
                     (process-package-version pkg version-idx version))
                   (iota (length (package-version* pkg)))
                   (package-version* pkg)))
       pkg-names pkgs)))

;; Write the lockfile.
(define (write-lockfile lockfile-filename projects dry-run?)
  (call-with-port (if dry-run?
                      (current-output-port)
                      (open-file-output-port
                       (string-append lockfile-filename ".tmp")
                       (file-options no-fail)
                       (buffer-mode block)
                       (native-transcoder)))
    (lambda (p)
      (display "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-\n" p)
      (display ";; This file is automatically generated - do not change it by hand.\n" p)
      (pretty-print `(import (akku format lockfile)) p)
      (pretty-print `(projects ,@projects) p)))
  (rename-file (string-append lockfile-filename ".tmp") lockfile-filename)
  (log/info "Wrote " lockfile-filename))

(define (lock-dependencies manifest-filename lockfile-filename index-filename)
  (define dry-run? #f)
  (define dev-mode? #t)
  (define manifest-packages (read-manifest manifest-filename))

  (let-values (((db packages) (read-package-index index-filename manifest-packages)))
    (add-package-dependencies db packages manifest-packages dev-mode?)
    (let-values (((version-scores initial-choices) (scores/choices db manifest-packages)))
      (let* ((universe (dummy-db->universe db))
             (solver (make-solver universe
                                  `((version-scores . ,version-scores)
                                    (initial-choices . ,initial-choices)))))
        (log/debug (dsp-universe universe))
        (let lp ()
          (let ((solution (find-next-solution! solver 10000)))
            (cond
              (solution
               (let ((projects
                      (choice-set->project-list packages
                                                manifest-packages
                                                initial-choices
                                                (solution-choices solution))))
                 (cond ((not (exists not projects))
                        (write-lockfile lockfile-filename projects dry-run?))
                       (else
                        ;; TODO: log what is bad about this solution.
                        (log/info "Rejected solution, trying the next...")
                        (lp)))))
              (else
               (error 'lock-dependencies "No acceptable solution - dependency hell"))))))))))
