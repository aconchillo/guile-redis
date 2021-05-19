;;; (redis utils) --- redis module for Guile.

;; Copyright (C) 2013-2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-redis. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (redis utils)
  #:use-module (redis connection)
  #:use-module (redis commands)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (read-reply
            send-commands
            receive-commands))

(define (redis-read-delimited conn)
  (let* ((sock (redis-socket conn))
         (str (read-delimited "\r" sock)))
    ;; Skip \n
    (read-char sock)
    str))

(define (build-list len proc)
  (let loop ((iter len) (result '()))
    (cond
     ((zero? iter) result)
     (else (loop (- iter 1) (cons (proc) result))))))

(define (read-reply conn)
  (let* ((sock (redis-socket conn))
         (c (read-char sock)))
    (case c
      ;; Status
      ((#\+) (redis-read-delimited conn))
      ;; Integer
      ((#\:) (string->number (redis-read-delimited conn)))
      ;; Bulk
      ((#\$)
       (let ((len (string->number (redis-read-delimited conn))))
         (if (> len 0) (redis-read-delimited conn) '())))
      ;; Multi-bulk
      ((#\*)
       (let ((len (string->number (redis-read-delimited conn))))
         (reverse (build-list len (lambda () (read-reply conn))))))
      ;; Error
      ((#\-)
       (let ((err (redis-read-delimited conn)))
         (throw 'redis-error err conn)))
      (else (throw 'redis-invalid conn)))))

(define (command->list cmd)
  (cons (redis-cmd-name cmd)
        (map (lambda (v)
               (cond
                ((string? v) v)
                ((symbol? v) (symbol->string v))
                ((number? v) (number->string v))
                (else (throw 'redis-invalid))))
             (redis-cmd-args cmd))))

(define (command->string cmd)
  (let ((l (command->list cmd)))
    (call-with-output-string
     (lambda (port)
       (format port "*~a\r\n" (length l))
       (for-each
        (lambda (e)
          (format port "$~a\r\n" (bytevector-length (string->utf8 e)))
          (format port "~a\r\n" e))
        l)))))

(define (send-command conn cmd)
  (let ((sock (redis-socket conn)))
    (put-string sock (command->string cmd))
    (force-output sock)))

(define (send-commands conn commands)
  (cond
   ((list? commands)
    (for-each
     (lambda (cmd) (send-command conn cmd))
     commands))
   (else (send-command conn commands))))

(define (receive-commands conn commands)
  (cond
   ((list? commands)
    (map
     (lambda (cmd)
       ((redis-cmd-reply cmd) conn))
     commands))
   (else
    ((redis-cmd-reply commands) conn))))

;;; (redis utils) ends here
