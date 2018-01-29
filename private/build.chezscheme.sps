#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later
#!r6rs

;;; Linux build script for Akku.scm

(import
  (chezscheme)
  (only (akku lib utils) string-split path-join mkdir/recursive)
  (semver versions))

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
  (call-with-input-file manifest
    (lambda (p) (read p) (cadadr (read p)))))

(define (copy-chez-notice target)
  ;; Apache License 2.0 requires LICENSE and NOTICE to be included in
  ;; redistributions.
  (cond ((and (file-exists? "/usr/share/doc/chezscheme/copyright")
              (file-exists? "/usr/share/doc/chezscheme/changelog.Debian.gz"))
         (cp "/usr/share/doc/chezscheme/copyright" target))
        ((let ((dir (or (getenv "CHEZSOURCEDIR")
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

(define (copy-notices target source)
  (call-with-output-file target
    (lambda (outp)
      (let-values (((to-stdin from-stdout from-stderr _process-id)
                    (open-process-ports
                     (string-append "bin/akku license-scan " source)
                     (buffer-mode block)
                     (native-transcoder))))
        (when (port-eof? from-stdout)
          (error 'copy-notices "Blank output from license-scan" source
                 (get-string-all from-stderr)))
        (put-string outp (get-string-all from-stdout))
        (display (get-string-all from-stderr))
        (close-port to-stdin)))))

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
      (long-machine-type
       (case (machine-type)
         ((a6le ta6le) "amd64-linux")
         ((i3le ti3le) "i386-linux")
         ((arm32le tarm32le) "arm-linux")
         (else
          (symbol->string (machine-type)))))
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
      ;; Licenses, copyrights, etc
      (copy-chez-notice "dist/doc/ChezScheme.txt")
      (guard (exn
              ((and (who-condition? exn)
                    (eq? (condition-who exn) 'copy-notices))
               (display "WARNING: Could not gather copyright notices\n"
                        (current-error-port))))
        (copy-notices "dist/doc/copyright.txt" "bin/akku.sps"))
      (cp "README.md" "dist/doc")
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
      (let* ((build-version (format #f "~d+~d" akku-version long-machine-type))
             (tarfile (format #f "akku-~d.tar.gz" build-version)))
        (assert (string->semver akku-version))
        (assert (string->semver build-version))
        (putenv "FILENAME" tarfile)
        (putenv "DISTVER" build-version)
        (system "tar --numeric-owner --owner 0 --group 0 -cvzf \"$FILENAME\" --transform s,^dist,akku-$DISTVER, dist/")
        (format #t "built ~d~%" tarfile)))))
