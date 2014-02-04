;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2014, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:lvsn-utils)

(defun use-package* (packages-to-use &optional (package *package*))
  (mklistf packages-to-use)
  (dolist (p packages-to-use t)
    (do-external-symbols (s p)
      (shadowing-import s package))))

(defun special-variable-p (symbol)
  (declare (type symbol symbol))
  (eq :special (sb-cltl2:variable-information symbol)))

(defun symbol-macro-p (symbol)
  (declare (type symbol symbol))
  (eq :symbol-macro (sb-cltl2:variable-information symbol)))

(defun symbol-attributes (symbol)
  (declare (type symbol symbol))
  (let ((attrs '()))
    (when (boundp symbol)
      (push :bound attrs))
    (cond ((constantp symbol)
           (push :constant attrs))
          ((special-variable-p symbol)
           (push :special-variable attrs))
          ((symbol-macro-p symbol)
           (push :symbol-macro attrs)))
    (cond ((special-operator-p symbol)
           (push :special-operator attrs))
          ((macro-function symbol)
           (push :macro attrs))
          (t (when (fboundp symbol)
               (if (typep (symbol-function symbol) 'generic-function)
                 (push :generic-function attrs)
                 (push :function attrs)))
             (when (compiler-macro-function symbol)
               (push :compiler-macro attrs))))
    (cond ((ignore-errors (subtypep symbol 'error))
           (push :error attrs))
          ((ignore-errors (subtypep symbol 'warning))
           (push :warning attrs))
          ((ignore-errors (subtypep symbol 'condition))
           (push :condition attrs))
          ((find-class symbol nil)
           (pushnew :class attrs))
          ((nth-value 1 (ignore-errors (subtypep symbol 'cons)))
           (push :type attrs)))
    (nreverse attrs)))

(defun list-external-symbols (package-designator)
  (let ((symbols '()))
    (do-external-symbols (s package-designator)
      (pushnew s symbols))
    (sort symbols #'string< :key #'symbol-name)))

(defun external-symbol-p (symbol)
  (let ((package (symbol-package symbol)))
    (if (not package)
      nil
      (do-external-symbols (s package)
        (when (eq s symbol)
          (return t))))))

(defun apropos-all (pattern &optional external-only)
  (dolist (s (if external-only
               (loop :for p :in (list-all-packages)
                     :nconc (apropos-list pattern p t))
               (apropos-list pattern)))
    (let ((attrs (symbol-attributes s)))
      (if (null attrs)
        (format t "~s~%" s)
        (format t "~s (~{~(~a~)~^, ~})~%" s attrs)))))

(defun apropos-package (package &optional (pattern "") (external-only t))
  (dolist (s (if external-only
               (apropos-list pattern package t)
               (apropos-list pattern package)))
    (let ((attrs (symbol-attributes s)))
      (if (null attrs)
        (format t "~s~%" s)
        (format t "~s (~{~(~a~)~^, ~})~%" s attrs)))))

;;;; vim: ft=lisp et
