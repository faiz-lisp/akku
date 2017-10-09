#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
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

(import
  (only (akku format lockfile) lockfile-filename)
  (only (akku format manifest) manifest-filename)
  (akku lib init)
  (akku lib install)
  (rnrs (6)))

(define (cmd-help)
  (display "akku - Scheme package manager

Usage:
 akku init - create a draft Akku.manifest
 akku install - install dependencies according to Akku.lock
" (current-error-port))
  (exit 0))

(define (cmd-install arg*)
  (unless (null? arg*)
    (cmd-help))
  (unless (file-exists? lockfile-filename)
    ;; XXX: create the manifest?
    (error 'install "The lockfile must exist before running this command"
           lockfile-filename))
  (install lockfile-filename 'dev))

(define (cmd-init arg*)
  (unless (null? arg*)
    (cmd-help))
  (when (file-exists? manifest-filename)
    ;; XXX: well... it should do something useful
    (error 'install "The manifest already exists" manifest-filename))
  (init-manifest manifest-filename "."))

(cond
  ((null? (cdr (command-line)))
   (cmd-help))
  ((string=? (cadr (command-line)) "install")
   (cmd-install (cddr (command-line))))
  ((string=? (cadr (command-line)) "init")
   (cmd-init (cddr (command-line))))
  (else
   (cmd-help)))
