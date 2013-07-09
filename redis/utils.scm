;;; (redis utils) --- redis module for Guile.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-redis; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (redis utils)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:export (read-error
            read-status
            read-integer))

(define (redis-read-delimited sock)
  (let ((str (read-delimited "\r" sock)))
    ;; Skip \n
    (read-char sock)
    str))

(define (read-error sock)
  (let ((err (redis-read-delimited sock)))
    (throw 'redis-error err)))

(define (read-status sock)
  (let ((c (read-char sock)))
    (case c
      ((#\+) (redis-read-delimited sock))
      ((#\-) (read-error sock))
      (else (throw 'redis-invalid)))))

(define (read-integer sock)
  (let ((c (read-char sock)))
    (case c
      ((#\:) (string->number (redis-read-delimited sock)))
      ((#\-) (read-error sock))
      (else (throw 'redis-invalid)))))
