#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
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
  (akku lib graph)
  (akku lib init)
  (akku lib install)
  (akku lib lock)                       ;here temporarily
  (rnrs (6)))

(define (cmd-graph . _)
  (print-gv-file "."))

(define (cmd-help)
  (display "akku - Scheme package manager

Basic usage:
 akku init - create a draft Akku.manifest (not yet useful)
 akku install - install dependencies according to Akku.lock

Advanced usage:
 akku graph - print a graphviz file showing library dependencies

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

(define (tmp-lock arg*)
  (unless (null? arg*)
    (cmd-help))
  (unless (file-exists? manifest-filename)
    (error 'install "The manifest must exist first" manifest-filename))
  (lock-dependencies manifest-filename
                     (string-append lockfile-filename ".tmp")
                     "index.db"))

(cond
  ((null? (cdr (command-line)))
   (cmd-help))
  ((string=? (cadr (command-line)) "graph")
   (cmd-graph (cddr (command-line))))
  ((string=? (cadr (command-line)) "install")
   (cmd-install (cddr (command-line))))
  ((string=? (cadr (command-line)) "init")
   (cmd-init (cddr (command-line))))
  ((string=? (cadr (command-line)) "lock")
   (tmp-lock (cddr (command-line))))
  (else
   (cmd-help)))
