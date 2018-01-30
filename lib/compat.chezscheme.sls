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
    (except (rnrs (6)) file-exists?)
    (only (chezscheme) cd mkdir chmod getenv putenv rename-file
          system process open-process-ports directory-list
          file-regular? file-directory? file-symbolic-link?
          pretty-print file-exists?))

  (define (file-exists/no-follow? filename)
    (file-exists? filename #f))

  (define (symlink from to)
    (putenv "AKKU_FROM" from)
    (putenv "AKKU_TO" to)
    (assert (zero? (system "ln -s -r \"$AKKU_FROM\" \"$AKKU_TO\"")))))
