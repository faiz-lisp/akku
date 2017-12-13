#!/usr/bin/env scheme-script
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

;;; Linux build script for Akku.scm

(import (chezscheme)
        (only (akku lib utils) string-split path-join mkdir/recursive))

(define (which filename)                ;same as which(1)
  (let lp ((dirs (string-split (or (getenv "PATH") "") #\:)))
    (if (null? dirs)
        #f
        (let ((fn (path-join (car dirs) filename)))
          (if (and (file-exists? fn)
                   (> (bitwise-and (get-mode fn) #o111) 0))
              fn
              (lp (cdr dirs)))))))

(define (cp src dst)
  (putenv "SRC" src)
  (putenv "DST" dst)
  ;; FIXME: do it in-process
  (system "set -x;/bin/cp \"$SRC\" \"$DST\""))

(define (ln/s src dst)
  (putenv "SRC" src)
  (putenv "DST" dst)
  (system "set -x;/bin/ln -s \"$SRC\" \"$DST\""))

(define (get-version manifest)
  (let ((version
         (symbol->string
          (call-with-input-file manifest
            (lambda (p) (read p) (cadadr (read p)))))))
    (substring version 1 (string-length version))))

(define (copy-chez-notice target)
  ;; Apache License 2.0 requires LICENSE and NOTICE to be included in
  ;; redistributions.
  (cond ((let ((dir (or (getenv "CHEZSOURCEDIR")
                        (begin
                          (display "Please enter the location of the Chez Scheme source: ")
                          (get-line (current-input-port))))))
           (and (string? dir) (file-directory? dir) dir))
         =>
         (lambda (srcdir)
           (call-with-output-file target
             (lambda (outp)
               (call-with-input-file (path-join srcdir "NOTICE")
                 (lambda (np) (put-string outp (get-string-all np))))
               (call-with-input-file (path-join srcdir "LICENSE")
                 (lambda (np) (put-string outp (get-string-all np))))))))
        (else
         ;; Not exactly the same files, but close enough.
         (format #t "Warning: downloading Chez Scheme LICENSE and NOTICE from github.~%")
         (format #t "Press enter to continue.~%")
         (get-line (current-input-port))
         (putenv "LICENSE_URL" "https://raw.githubusercontent.com/cisco/ChezScheme/master/LICENSE")
         (putenv "NOTICE_URL" "https://raw.githubusercontent.com/cisco/ChezScheme/master/NOTICE")
         (putenv "TARGET_FILE" target)
         (system "curl \"$NOTICE_URL\" \"$LICENSE_URL\" > \"$TARGET_FILE\"")))
  (assert (file-exists? target)))

(assert (not (petite?)))

;; Compile the akku binary.
(let-values (((from-stdout to-stdin _) (apply values (process "scheme -q"))))
  ;; Compilation is done in a subprocess, because already loaded
  ;; libraries are not compiled again, which means no wpo files.
  (delete-file "bin/akku")
  (write '(parameterize ((compile-imported-libraries #t)
                         (generate-wpo-files #t)
                         (optimize-level 2))
            (compile-program "bin/akku.sps" "bin/akku.so")
            (let ((remaining (compile-whole-program "bin/akku.wpo" "bin/akku" #f)))
              (unless (null? remaining)
                (when (file-exists? "bin/akku")
                  (delete-file "bin/akku"))
                (error 'build "Some libraries were not compiled" remaining))))
         to-stdin)
  (close-port to-stdin)
  (display (get-string-all from-stdout))
  (close-port from-stdout)
  (chmod "bin/akku" #o755)
  (format #t "built bin/akku~%"))

;; Build the distribution.
(system "/bin/rm -rf dist")
(let ((chez-version (car (reverse (string-split (scheme-version) #\space))))
      (machine (symbol->string (machine-type)))
      (akku-version (get-version "Akku.manifest")))
  ;; Chez doesn't provide an easy way to find the default boot files...
  (let ((default-heap-path (format #f "lib/csv~d/~d" chez-version machine))
        (petite (which "petite")))
    (unless petite
      (error 'build "Could not find petite in PATH"))
    (let* ((heap-dir (path-join (path-parent (path-parent petite)) default-heap-path))
           (petite-boot (path-join heap-dir "petite.boot")))
      (unless (file-exists? petite-boot)
        (error 'build "Could not find petite.boot"))
      (format #t "bundling with ~d and ~d~%" petite petite-boot)
      (mkdir/recursive (path-join "dist/boot" machine))
      (mkdir/recursive (path-join "dist/bin" machine))
      (mkdir/recursive "dist/doc")
      ;; Petite boot file
      (let ((dist-petite-boot (format #f "dist/boot/~d/petite.boot" machine)))
        (cp petite-boot dist-petite-boot)
        (chmod dist-petite-boot #o644))
      (ln/s "petite.boot" (format #f "dist/boot/~d/scheme-script.boot" machine))
      ;; Petite binary
      (let ((dist-petite (format #f "dist/bin/~d/petite" machine)))
        (cp petite dist-petite)
        (chmod dist-petite #o755))
      (ln/s "petite" (format #f "dist/bin/~d/scheme-script" machine))
      (cp "bin/akku" (format #f "dist/bin/~d/akku"  machine))
      ;; Licenses and copyrights
      (cp "COPYING" "dist/doc")
      (copy-chez-notice "dist/doc/ChezScheme.txt")
      ;; Install script
      (call-with-output-file "dist/install.sh"
        (lambda (p)
          (format p "#!/bin/sh~%")
          (format p "PREFIX=$HOME/.akku~%")
          (format p "MACHINE=~d~%" machine)
          (format p "mkdir -p $PREFIX~%")
          (format p "cp -a bin boot $PREFIX/~%")
          (format p "cat > $PREFIX/bin/akku << EOF~%")
          (format p "#!/bin/sh~%")
          (format p "exec $PREFIX/bin/$MACHINE/petite -b $PREFIX/boot/$MACHINE/petite.boot --program $PREFIX/bin/$MACHINE/akku \\$*~%")
          (format p "EOF~%")
          (format p "chmod 0755 $PREFIX/bin/akku~%")
          (format p "mkdir -p $HOME/bin~%")
          (format p "test -f $HOME/bin/akku || ln -s $PREFIX/bin/akku $HOME/bin/~%")
          (format p "echo You can now run '~~/bin/akku'~%")))
      (chmod "dist/install.sh" #o755)
      ;; Build a tarball.
      (let* ((build-version (format #f "~d+pcsv~d.~d" akku-version chez-version machine))
             (tarfile (format #f "akku-~d.tar.gz" build-version)))
        (putenv "FILENAME" tarfile)
        (putenv "DISTVER" build-version)
        (system "tar -cvzf \"$FILENAME\" --transform s,^dist,akku-$DISTVER, dist/")
        (format #t "built ~d~%" tarfile)))))
