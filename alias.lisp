;; alias.lisp
;; Copyright (C) 2024  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 
;; Refs
;; https://stackoverflow.com/questions/69972590/apply-or-funcall-for-macros-instead-of-functions
;; https://stackoverflow.com/questions/8918373/dynamic-function-generation-with-macros
;;; Code:

(in-package :cl-user)

(defvar alias-table (make-hash-table))

(defun defalias (symbol target &optional docs)
  (when (functionp target)
    (setf target
          (caddr
           (multiple-value-list
            (function-lambda-expression target)))))
  (cond
    ((or (special-operator-p target)
         (macro-function target))
     (eval `(defmacro ,symbol (&body body) (list* ',target body))))
    ((fdefinition target)
     (when (or (special-operator-p symbol)
               (macro-function symbol))
       (fmakunbound symbol))
     (setf (symbol-function symbol)
           #'(lambda (&rest args)
               (let* ((p (symbol-package target))
                      (s (find-symbol (symbol-name target) p)))

                 (unless p (error "Package: ~S is not found" p))
                 (unless s (error "Symbol: ~S is not found" s))

                 (cond
                   ((special-operator-p s)
                    (format t "Cannot call special operators dynamically~%"))
                   ((macro-function s)
                    (format t "Cannot call macros dynamically~%"))
                   (t                    
                    (if args
                        (apply (symbol-function s) args)
                        (funcall (symbol-function s))))))))))
  (setf (documentation symbol 'function)
        (or docs (documentation target 'function)))
  target)

;;; alias.lisp ends here
