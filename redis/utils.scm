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
  #:use-module (redis commands define)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (send-commands
            read-error
            read-status
            read-integer
            read-bulk
            receive-commands))

(define (command->list cmd)
  (cons (redis-cmd-name cmd) (redis-cmd-params cmd)))

(define (command->string cmd)
  (let ((l (command->list cmd)))
    (call-with-output-string
     (lambda (port)
       (simple-format port "*~a\r\n" (length l))
       (for-each
        (lambda (e)
          (simple-format port "$~a\r\n" (bytevector-length (string->utf8 e)))
          (simple-format port "~a\r\n" e))
        l)))))

(define (send-command sock cmd)
  (display (command->string cmd) sock)
  (force-output sock))

(define (send-commands sock commands)
  (cond
   ((list? commands)
    (for-each
       (lambda (cmd) (send-command sock cmd))
       commands))
   (else (send-command sock commands))))

(define (receive-commands sock commands)
  (cond
   ((list? commands)
    (map
     (lambda (cmd)
       ((redis-cmd-reply cmd) sock))
     commands))
   (else
    ((redis-cmd-reply commands) sock))))

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

(define (read-bulk sock)
  (let ((c (read-char sock)))
    (case c
      ((#\$)
       (let ((len (string->number (redis-read-delimited sock))))
         (if (> len 0) (redis-read-delimited sock) #nil)))
      ((#\-) (read-error sock))
      (else (throw 'redis-invalid)))))

;;; (redis utils) ends here
