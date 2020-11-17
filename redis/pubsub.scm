;;; (redis pubsub) --- redis module for Guile.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

(define-module (redis pubsub)
  #:use-module (redis commands)
  #:use-module (redis utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (redis-publish
            redis-subscribe
            redis-psubscribe
            redis-unsubscribe
            redis-punsubscribe
            redis-subscribe-read))

(define (redis-publish connection channel message)
  "Publish the given @var{message} to @var{channel} using the provided Redis
client @var{connection}. All subscribers to that channel will receive the
message. Returns the number of clients that received the message."
  (send-commands connection (publish channel message))
  (read-reply connection))

(define* (redis-subscribe connection channels)
  "Subscribe to the given list of @var{channels} using the Redis client
@var{connection}. Returns a list of pairs where the first value is the
subscribed channel and the second value is the total number of subscribed
channels (when that channel was subscribed)."
    (redis--subscribe connection (subscribe channels)))

(define* (redis-unsubscribe connection #:optional channels)
  "Unsubscribe from the given optional list of @var{channels} using the Redis
client @var{connection}. If @var{channels} is not specified it will
unsubscribe from all the subscribed channels. Returns a list of pairs where
the first value is the unsubscribed channel and the second value is the total
number of remaining subscribed channels (when that channel was unsubscribed)."
  (redis--unsubscribe connection (unsubscribe (or channels '())) "unsubscribe"))

(define* (redis-psubscribe connection patterns)
  "Subscribe to the given list of channel @var{patterns} using the Redis
client @var{connection}. Returns a list of pairs where the first value is the
subscribed pattern and the second value is the total number of subscribed
channel patterns (when that pattern was subscribed)."
    (redis--subscribe connection (psubscribe (or patterns '()))))

(define* (redis-punsubscribe connection #:optional patterns)
  "Unsubscribe from the given optional list of channel @var{patterns} using
the Redis client @var{connection}. If @var{channels} is not specified it will
unsubscribe from all the subscribed patterns. Returns a list of pairs where
the first value is the unsubscribed pattern and the second value is the total
number of remaining subscribed channel patterns (when that patterns was
unsubscribed)."
  (redis--unsubscribe connection (punsubscribe (or patterns '())) "punsubscribe"))

(define (redis-subscribe-read connection)
  "Read the next message from one of the subscribed channels on the given
@var{connection}. This is a blocking operation until a message is received. It
returns a couple of values: the channel where the message was received and the
actual message."
  (let ((reply (read-reply connection)))
    (match reply
      (("message" channel message) (values channel message))
      (("pmessage" pattern channel message) (values channel message))
      (("PONG") "PONG")
      (_ (throw 'redis-invalid connection reply)))))

;; Internal

(define (publish channel message)
  (apply create-command "PUBLISH" (list channel message)))

(define (subscribe channels)
  (apply create-command "SUBSCRIBE" channels))

(define* (unsubscribe #:optional channels)
  (apply create-command "UNSUBSCRIBE" (if channels channels #nil)))

(define (psubscribe patterns)
  (apply create-command "PSUBSCRIBE" patterns))

(define* (punsubscribe #:optional patterns)
  (apply create-command "PUNSUBSCRIBE" (if patterns patterns #nil)))

(define* (redis--subscribe connection command)
  (send-commands connection command)
  (fold (lambda (res prev)
          (match-let (((type channel count) (read-reply connection)))
            (cons (cons channel count) prev)))
        '() (redis-cmd-args command)))

(define* (redis--unsubscribe connection command expected)
  (define (read-multiple-messages continue?)
    (let loop ((reply (read-reply connection))
               (result '())
               (remaining (redis-cmd-args command)))
      (match reply
        ((expected channel count)
         (cond ((continue? count remaining)
                (loop (read-reply connection)
                      (cons (cons channel count) result)
                      (if (pair? remaining) (cdr remaining) '())))
               (else (cons (cons channel count) result))))
        (_ (throw 'redis-invalid connection reply)))))
  (send-commands connection command)
  (if (pair? (redis-cmd-args command))
      (read-multiple-messages (lambda (count remaining) (pair? remaining)))
      (read-multiple-messages (lambda (count remaining) (> count 0)))))

;;; (redis pubsub) ends here
