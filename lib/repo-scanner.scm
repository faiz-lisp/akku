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

;; Scheme repository scanner.

(library (akku lib repo-scanner)
  (export
    scm-origin
    scm-file-list
    find-artifacts)
  (import
    (rnrs (6))
    (akku extern match)
    (akku lib compat)
    (akku lib git)
    (akku lib file-parser)
    (akku lib utils))

(define *verbose* #f)

;; Get the location for cloning the repository.
(define (scm-origin dir)
  (if (is-git-repository? dir)
      `(git ,"https://example.com")
      #f))

;; Get a list of files that are tracked by the scm system.
(define (scm-file-list dir)
  (cond ((is-git-repository? dir)
         (git-ls-files dir))
        (else #f)))

(define (basename->library-component filename)
  ;; TODO: unquote %-coding
  (string->symbol filename))

(define (filename->component filename)
  (match (string-split filename #\.)
    ((lib-name (or "ss" "sls" "scm" "sch" "sld")) (string->symbol lib-name))
    ((lib-name _impl "sls") (string->symbol lib-name))
    ((program-name "sps") (string->symbol program-name))
    ((program-name _impl "sps") (string->symbol program-name))
    ((program-name) (string->symbol program-name))
    (else #f)))

;; Takes a directory name and a list of files contained in it. Returns
;; a list of artifact records.
(define (find-artifacts* realpath relpath relpath-list files tracked-files)
  (define (filename->record* fn)
    (let ([realpath (path-join realpath fn)]
          [relpath (path-join relpath fn)])
      (cond
        ((or (string-prefix? "." fn)
             (string-suffix? "~" fn)
             (string=? "_darcs" fn)
             (string=? "Akku.lock" fn)
             (string=? "Akku.manifest" fn)
             (file-symbolic-link? fn))
         (when *verbose*
           (print ";; Ignored " relpath))
         '())                        ;ignore
        ((file-regular? realpath)
         (let ([path-list (reverse (cons (filename->component fn) relpath-list))])
           (cond
             ((and (pair? tracked-files) (not (member relpath tracked-files)))
              (when *verbose*
                (print ";; File " relpath " is not a tracked file, ignored."))
              '())
             ((exists (lambda (x) (not x)) path-list)
              (when *verbose*
                (print ";; File " relpath " rejected by filename->component"))
              (list (make-generic-file relpath path-list)))
             ((examine-file realpath relpath path-list))
             (else
              (when *verbose*
                (print ";; File " relpath " rejected by examine-file"))
              (list (make-generic-file relpath path-list))))))
        ((file-directory? realpath)
         (find-artifacts* realpath relpath
                          (cons (basename->library-component fn) relpath-list)
                          (directory-list realpath)
                          tracked-files))
        (else
         (when *verbose*
           (print ";; Ignored " relpath " because it is not a regular file or directory"))
         '()))))
  (append-map filename->record* (list-sort string<? files)))

(define (find-artifacts directory tracked-files)
  (find-artifacts* directory "" '()
                   (directory-list directory)
                   tracked-files)))
