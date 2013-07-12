;;; (redis commands strings) --- redis module for Guile.

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

(define-module (redis commands strings)
  #:use-module (redis utils)
  #:use-module (redis commands define)
  #:export (append bitcount bitop decr decrby get
            getbit getrange getset incr incrby incrbyfloat
            mget mset msetnx psetex set setbit
            setex setnx setrange strlen))

(define (append key value)
  (make-command "APPEND" key value))

(define* (bitcount key #:optional (start 0) (end -1))
  (make-command "BITCOUNT" key (number->string start) (number->string end)))

(define* (bitop operation destkey key #:rest keys)
  (make-command "BITOP" operation destkey key keys))

(define (decr key)
  (make-command "DECR" key))

(define (decrby key decrement)
  (make-command "DECRBY" key decrement))

(define (get key)
  (make-command "GET" key))

(define (getbit key offset)
  (make-command "GETBIT" key offset))

(define (getrange key start end)
  (make-command "GETRANGE" key start end))

(define (getset key value)
  (make-command "GETSET" key value))

(define (incr key)
  (make-command "INCR" key))

(define (incrby key increment)
  (make-command "INCRBY" key increment))

(define (incrbyfloat key increment)
  (make-command "INCRBYFLOAT" key increment))

(define* (mget key #:rest keys)
  (apply make-command `("MGET" ,key ,@keys)))

(define* (mset key value #:rest pairs)
  (apply make-command `("MSET" ,key ,value ,@pairs)))

(define* (msetnx key value #:rest pairs)
  (apply make-command `("MSETNX" ,key ,value ,@pairs)))

(define (psetex key milliseconds value)
  (make-command "PSETEX" key milliseconds value))

(define (set key value)
  (make-command "SET" key value))

(define (setbit key offset value)
  (make-command "SETBIT" key offset value))

(define (setex key seconds value)
  (make-command "SETEX" key seconds value))

(define (setnx key value)
  (make-command "SETNX" key value))

(define (setrange key offset value)
  (make-command "SETRANGE" key offset value))

(define (strlen key)
  (make-command "STRLEN" key))

;;; (redis commands strings) ends here
