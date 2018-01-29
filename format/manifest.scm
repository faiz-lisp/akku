;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Definition of the manifest format.

(library (akku format manifest)
  (export
    manifest-filename
    akku-package)
  (import
    (rnrs))

(define manifest-filename "Akku.manifest")

(define-syntax akku-package
  (lambda (x)
    (syntax-case x (description authors license notice-files extra-files
                                source depends depends/dev)
      ((_ . x*)
       #f))))

;; (define-syntax name
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ (id ...))
;;        (for-all (lambda (component)
;;                   (or (symbol? (syntax->datum component))
;;                       (number? (syntax->datum component))))
;;                 #'(id ...)))
;;       ((_ name)
;;        (string? (syntax->datum #'name))))))

;; (define-syntax version
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ version)
;;        (string? (syntax->datum #'version))))))

;; (define-syntax license
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ spdx-id)
;;        (string? (syntax->datum #'spdx-id))))))

;; (define-syntax private
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_)
;;        #'#f))))

;; (define-syntax depends
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ . pkg*) #'#f))))

;; (define-syntax depends/dev
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ . pkg*) #'#f))))
)
