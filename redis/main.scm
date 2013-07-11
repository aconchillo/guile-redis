;;; (redis main) --- redis module for Guile.

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

(define-module (redis main)
  #:use-module (redis utils)
  #:use-module (srfi srfi-9)
  #:export (redis-connect
            redis-connection?
            redis-host
            redis-port
            redis-socket
            redis-close
            redis-send))

(define-record-type <redis-connection>
  (make-connection host port sock)
  redis-connection?
  (host redis-host)
  (port redis-port)
  (sock redis-socket))

(define* (redis-connect #:key (host "127.0.0.1") (port 6379))
  "Establish a connection to the redis server at the given @var{host}
and @var{port}. The @var{port} defaults to 6379. Return a redis
connection."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (connect sock AF_INET (inet-pton AF_INET host) port)
    (make-connection host port sock)))

(define (redis-close conn)
  "Close the @var{connection} to the redis server."
  (shutdown (redis-socket conn) 2))

(define (redis-send conn commands)
  "Send the given list of @var{commands} to the redis connection
@var{conn}. @var{commands} can be a single command or a list of
commands."
  (let ((sock (redis-socket conn)))
    (send-commands sock commands)
    (receive-commands sock commands)))

;;; (redis main) ends here
