;;; (redis) --- Redis module for Guile.

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

(define-module (redis)
  #:use-module (redis main)
  #:use-module (redis commands connection))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (redis main)
                   (redis commands connection))

;;; (json) ends here
