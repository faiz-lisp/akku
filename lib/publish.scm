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

;; Publish packages.

(library (akku lib publish)
  (export
    publish-packages)
  (import
    (rnrs (6))
    (semver versions)
    (srfi :115 regexp)
    (wak fmt)
    (only (xitomatl common) pretty-print)
    (akku lib git)
    (akku lib manifest)

    )


#|
URL=$(git remote -v|grep fetch|awk '{print $2}'|head -1)
PKG_NAME=$(basename $PWD)

#git tag -l|sed 's/^v//'|xargs semver|tail -n1
TAG=$(git tag -l|grep ^v|tail -n1)

if [ "x$TAG" = "x" ]; then

COMMITS=$(git log|grep ^Date|wc -l)
SHORT=$(git describe --always)
REVISION=$(git rev-parse HEAD)
cat <<EOF
(package (name "$PKG_NAME")
  (versions
   ((version "0.0.0-akku.$COMMITS.$SHORT")
    (lock (location (git "$URL"))
          (revision "$REVISION")))))
EOF

else

REVISION=$(git rev-list -n 1 "$TAG")
VERSION=$(echo $TAG | sed 's/v//')
cat <<EOF
(package (name "$PKG_NAME")
  (versions
   ((version "$VERSION")
    (lock (location (git "$URL"))
          (tag "$TAG")
          (revision "$REVISION")))))
EOF

fi

  |#


(define (guess-public-git-location base-directory)
  (let lp ((remote* (git-list-remotes base-directory)))
    (if (null? remote*)
        #f
        (let ((url (git-remote-get-url base-directory (car remote*))))
          (cond
            ;; https://github.com/weinholt/akku.git
            ((regexp-matches (rx "https://" ($ (+ any))) url)
             => (lambda (m) url))
            ;; git@github.com:weinholt/akku.git
            ((regexp-matches (rx "git@" ($ (or "github.com" "gitlab.com"))
                                 ":" ($ (+ any))) url)
             => (lambda (m)
                  (string-append "https://" (regexp-match-submatch m 1) "/"
                                 (regexp-match-submatch m 2))))
            (else
             (lp (cdr remote*))))))))

;; Finds information to put into the lock part of the package index.
(define (get-git-lock base-directory version)
  (let ((version-tags (git-tag-list base-directory "v*"))
        (version-tag (string-append "v" version))
        (head-revision (git-rev-parse base-directory "HEAD"))
        (pull-url (guess-public-git-location base-directory)))
    (unless pull-url
      (error 'get-git-lock "The git location must be added to Akku.manifest"))
    (cond ((member version-tag version-tags)
           (let ((revision (git-rev-parse base-directory version-tag)))
             (unless (string=? head-revision revision)
               (fmt (current-error-port)
                    ";; WARNING: Branch HEAD does not match the release tag" nl))
             `((location (git ,pull-url))
               (tag ,version-tag)
               (revision ,revision))))
          (else
           (fmt (current-error-port) "INFO: Publishing untagged release" nl)
           `((location (git ,pull-url))
             (revision ,head-revision))))))

(define (publish-packages/git packages base-directory)
  (let* ((version (car (package-version* (car packages))))
         (lock (get-git-lock base-directory (version-number version))))
    (for-each (lambda (pkg)
                (version-lock-set! version lock))
              packages)
    (for-each (lambda (pkg)
                (fmt #t (pretty (package->index-package pkg))))
              packages)
    (newline)))

(define (publish-packages manifest-filename base-directory)
  (cond
    ((file-exists? manifest-filename)
     (let ((packages (read-manifest manifest-filename)))
       (when (null? packages)
         (error 'publish-packages "Empty manifest"))
       (cond
         ((is-git-repository? base-directory)
          (publish-packages/git packages base-directory))
         (else
          (error 'publish-packages
                 "Publishing from non-git repositories is not yet supported")))))
    (else
     (write-manifest manifest-filename
                     (list (draft-akku-package #f ;XXX: use highest version tag
                                               `(location (git "https://example.com/")))))
     (fmt #t "Edit " manifest-filename " and run publish again" nl))))


)
