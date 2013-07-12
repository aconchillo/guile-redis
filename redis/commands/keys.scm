;;; (redis commands keys) --- redis module for Guile.

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

(define-module (redis commands keys)
  #:use-module (redis commands define)
  #:export (del dump  exists expire expireat
            keys migrate move object persist
            pexpire pexpireat pttl randomkey rename
            renamenx restore sort ttl type))

(define (del keys)
  (apply make-command `("DEL" ,@keys)))

(define (dump key)
  (make-command "DUMP" key))

(define (exists key)
  (make-command "EXISTS" key))

(define (expire key seconds)
  (make-command "EXPIRE" key (number->string seconds)))

(define (expireat key timestamp)
  (make-command "EXPIREAT" key (number->string timestamp)))

(define (keys pattern)
  (make-command "KEYS" pattern))

(define (migrate host port key destination-db timeout)
  (make-command "MIGRATE"
                host (number->string port)
                key (number->string destination-db) (number->string timeout)))

(define (move key db)
  (make-command "MOVE" key (number->string db)))

(define* (object subcommand #:rest arguments)
  (case subcommand
    ((REFCOUNT)
     (apply make-command `("OBJECT" "REFCOUNT" ,@arguments)))
    ((ENCODING)
     (apply make-command `("OBJECT" "ENCODING" ,@arguments)))
    ((IDLETIME)
     (apply make-command `("OBJECT" "IDLETIME" ,@arguments)))
    (else (throw 'redis-error "Invalid OBJECT subcommand"))))

(define (persist key)
  (make-command "PERSIST" key))

(define (pexpire key milliseconds)
  (make-command "PEXPIRE" key (number->string milliseconds)))

(define (pexpireat key ms-timestamp)
  (make-command "PEXPIREAT" key (number->string ms-timestamp)))

(define (pttl key)
  (make-command "PTTL" key))

(define (randomkey)
  (make-command "RANDOMKEY"))

(define (rename key newkey)
  (make-command "RENAME" key newkey))

(define (renamenx key newkey)
  (make-command "RENAMENX" key newkey))

(define (restore key ttl value)
  (make-command "RESTORE" key (number->string ttl) value))

;; TODO add extra arguments
(define (sort key)
  (make-command "SORT" key))

(define (ttl key)
  (make-command "TTL" key))

(define (type key)
  (make-command "TYPE" key))

;;; (redis commands keys) ends here
