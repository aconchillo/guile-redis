;;; (redis commands define) --- redis module for Guile.

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

;; The following is a convenient Emacs interactive function to create
;; new Redis command definitions. Uncomment it temporarily, go to the
;; end of the function and M-x eval-fun.

;; Once the function is loaded in Emacs, M-x redis-add-command.

;; (defun redis-add-command ()
;;   (interactive)
;;   (let ((command (read-string "Command: "))
;;         (args (read-string "Arguments: "))
;;         (opt-args (read-string "Optional arguments: "))
;;         (rest-args (read-string "Rest arguments: "))
;;         (reply (completing-read "Reply function: "
;;                                 '(("read-status" 1)
;;                                   ("read-integer" 2)
;;                                   ("read-bulk" 3)
;;                                   ("read-multi-bulk" 4))
;;                                 nil t)))
;;     (cond
;;      ((or (> (length opt-args) 0) (> (length rest-args) 0))
;;       (insert "(define* (" command)
;;       (if (> (length args) 0) (insert " " args))
;;       (if (> (length opt-args) 0) (insert " #:optional " opt-args))
;;       (if (> (length rest-args) 0) (insert " #:rest " rest-args)))
;;      (t
;;       (insert "(define (" command)
;;       (if (> (length args) 0) (insert " " args))))
;;     (insert ")\n")
;;     (insert "  (make-command \"" (upcase command) "\" " reply)
;;     (if (> (length args) 0) (insert " " args))
;;     (if (> (length opt-args) 0) (insert " " opt-args))
;;     (if (> (length rest-args) 0) (insert " " rest-args))
;;     (insert "))\n")))

;;; Code:

(define-module (redis commands define)
  #:use-module (srfi srfi-9)
  #:export (make-command
            redis-command?
            redis-cmd-name
            redis-cmd-params
            redis-cmd-reply))

(define-record-type <redis-command>
  (create-command name params reply)
  redis-command?
  (name redis-cmd-name)
  (params redis-cmd-params)
  (reply redis-cmd-reply))

(define* (make-command name reply #:rest args)
  (create-command name args reply))

;;; (redis commands define) ends here
