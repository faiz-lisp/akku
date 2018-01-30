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

;; XXX: could use (xitomatl file-system base compat)

(library (akku lib compat)
  (export
    cd
    mkdir
    chmod
    rename-file
    symlink
    getenv
    putenv
    system
    process
    open-process-ports
    directory-list
    file-regular?
    file-directory?
    file-symbolic-link?
    file-exists/no-follow?
    pretty-print)
  (import
    (rnrs (6)))

(define (cd new-directory)
  (display "cd " (current-error-port))
  (display new-directory (current-error-port))
  (newline (current-error-port)))

(define (mkdir path)
  (display "mkdir " (current-error-port))
  (display path (current-error-port))
  (newline (current-error-port)))

(define (chmod path mode)
  (display "chmod " (current-error-port))
  (display (number->string mode 8) (current-error-port))
  (display " " (current-error-port))
  (display path (current-error-port))
  (newline (current-error-port)))

(define (rename-file old new)               ;atomic overwriting file rename
  (error 'rename-file "Please implement (akku lib compat)"))

(define (getenv name)
  (error 'getenv "Please implement (akku lib compat)"))

(define (putenv name value)
  (display "export " (current-error-port))
  (display name (current-error-port))
  (display #\= (current-error-port))
  (display value (current-error-port))
  (newline (current-error-port)))

(define (system cmd)
  (display cmd (current-error-port))
  (newline (current-error-port))
  0)

(define (process _command)
  (error 'process "Please implement (akku lib compat)"))

(define (open-process-ports _command)
  (error 'open-process-ports "Please implement (akku lib compat)"))

(define (directory-list _path)
  (error 'directory-list "Please implement (akku lib compat)"))

(define (file-regular? _path)
  (error 'file-regular? "Please implement (akku lib compat)"))

(define (file-directory? _path)
  (error 'file-directory? "Please implement (akku lib compat)"))

(define (file-symbolic-link? _path)
  (error 'file-symbolic-link? "Please implement (akku lib compat)"))

(define (file-exists/no-follow? _path)
  (error 'file-exists/no-follow? "Please implement (akku lib compat)"))

(define (symlink from to)
  (error 'symlink "Please implement (akku lib compat)"))

(define pretty-print
  (case-lambda
    ((datum port)
     (write datum port)
     (newline port))
    ((datum)
     (pretty-print datum (current-output-port))))))
