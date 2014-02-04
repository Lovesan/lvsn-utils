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

(defmacro defhelper (name (&rest args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,args ,@body)))

(defhelper mklist (x)
  (if (listp x) x (list x)))

(define-modify-macro mklistf () mklist)

(defhelper mkstring (x)
  (etypecase x
    (string x)
    ((or symbol character) (string x))
    (pathname (namestring x))
    (real (format nil "~a" x))))

(define-modify-macro mkstringf () mkstring)

(defhelper mkgensym (x)
  (gensym (mkstring x)))

(define-modify-macro mkgensymf () mkgensym)

(defmacro dolist* ((var list) &body body)
  `(mapcar (lambda (,var) ,@body) ,list))

(defmacro with-gensyms ((&rest symbols) &body body)
  (let ((bindings (dolist* (s symbols)
                    (destructuring-bind
                      (var &optional (name var))
                        (mklist s)
                      `(,var (mkgensym ',name))))))
    `(let ,bindings
       ,@body)))

(defmacro doseq ((var sequence &optional return-form) &body body)
  (with-gensyms (i length tag seq)
    `(prog* ((,seq ,sequence)
             (,length (length ,seq))
             (,i 0))
        (declare (type index ,i ,length))
        ,tag
        (unless (< ,i ,length)
          (return ,return-form))
        (let ((,var (elt ,seq ,i)))
          ,@body)
        (incf ,i)
        (go ,tag))))

(defmacro doseq* ((var sequence) &body body)
  (with-gensyms (seq)
    `(let ((,seq ,sequence))
       (map (type-of ,seq) (lambda (,var) ,@body) ,seq))))

(defmacro do-map-reduce (list
                         ((map-var) &body map-body)
                         ((reduce-acc reduce-var) &body reduce-body))
  `(reduce (lambda (,reduce-acc ,reduce-var) ,@reduce-body)
           (mapcar (lambda (,map-var) ,@map-body) ,list)))

(defmacro with-collect (&body body)
  (with-gensyms (head tail thing)
    `(let* ((,tail (cons nil nil))
            (,head ,tail))
       (declare (type cons ,tail ,head))
       (flet ((collect (,thing)
                (setf (cdr ,tail) (list ,thing)
                      ,tail (cdr ,tail))
                ,thing))
         ,@body
         (cdr ,head)))))

;;;; vim: ft=lisp et
