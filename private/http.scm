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

;; HTTP client.

(library (akku private http)
  (export
    make-http-request http-request?
    http-request-method
    http-request-url
    open-http-request
    http-response?
    http-response-status
    http-response-port)
  (import
    (rnrs (6))
    (srfi :115 regexp)
    (akku lib compat))

(define-record-type http-request
  (nongenerative)
  (sealed #t)
  (fields method url))

(define-record-type http-response
  (nongenerative)
  (sealed #t)
  (fields status port))

;; TODO: Use a native HTTP client (ocelotl) when net libraries are
;; working more reasonably.

(define (open-http-request req)
  (putenv "AKKU_METHOD" (case (http-request-method req)
                          ((get) "GET")
                          ((post) "POST")
                          ((put) "PUT")
                          ((delete) "DELETE")
                          ((patch) "PATCH")
                          (else
                           (assertion-violation 'open-http-request "Bad method" req))))
  (putenv "AKKU_URL" (http-request-url req))
  (let-values (((to-stdin from-stdout from-stderr _process-id)
                (open-process-ports "exec curl -s -X \"$AKKU_METHOD\" -D /dev/stderr \"$AKKU_URL\"")))
    (close-port to-stdin)
    (let* ((stderr (transcoded-port from-stderr (make-transcoder (utf-8-codec)
                                                                 (eol-style lf))))
           (resp (get-line stderr)))
      (close-port from-stderr)
      (cond ((regexp-matches (rx "HTTP/" (+ (~ " ")) " " (submatch (+ (~ " "))) (* any))
                             resp)
             => (lambda (m)
                  (let ((status (regexp-match-submatch m 1)))
                    (make-http-response status from-stdout))))
            (else
             (close-port from-stdout)
             (error 'open-http-request "Could not open request" req resp)))))))
