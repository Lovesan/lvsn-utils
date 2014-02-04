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

(defconstant +default-hash-size+ 16)

(defstruct (hash (:constructor make-hash ())
                 (:conc-name %hash-))
  "Simple generic hash table"
  (buckets (make-array +default-hash-size+ :initial-element nil)
   :type simple-vector)
  (elt-count 0 :type index)
  (threshold (/ 1 1.5) :type single-float))

(defmethod print-object ((hash hash) stream)
  (with-accessors ((buckets %hash-buckets)
                   (count %hash-elt-count)
                   (threshold %hash-threshold))
      hash
    (print-unreadable-object (hash stream :type t :identity t)
      (pprint-logical-block (stream nil)
        (format stream "element count: ~d " count)
        (pprint-newline :mandatory stream)
        (format stream "size ratio: ~,2f "
                (if (zerop count)
                  0.0
                  (/ count (float (length buckets)))))
        (pprint-newline :mandatory stream)
        (format stream "threshold: ~,2f " threshold))))
  hash)

(declaim (ftype (function (t t) t) hasheq))

(defgeneric hasheq (x y)
  (:documentation
"Hash equality test function.
 (hasheq x y) -> (= (hash x) (hash y))")
  (:method (x y)
    (equal x y)))

(defgeneric hash (x)
  (:documentation
"Hash code function.
 (hasheq x y) -> (= (hash x) (hash y))")
  (:method (x)
    (sxhash x)))

(defun hash-grow (hash)
  (declare (type hash hash))
  (let* ((buckets (%hash-buckets hash))
         (new-size (min (1+ (* (length buckets) 2))
                        (1- array-total-size-limit)))
         (new-buckets (make-array new-size :initial-element nil)))
    (doseq (b buckets)
      (dolist (pair b)
        (let* ((code (the ufixnum (hash (car pair))))
               (index (mod code new-size)))
          (push pair (svref new-buckets index)))))
    (setf (%hash-buckets hash) new-buckets)
    hash))

(defun hashref (hash key &optional default)
  "Gets value from hash table by key"
  (declare (type hash hash))
  (let* ((buckets (%hash-buckets hash))
         (index (mod (the ufixnum (hash key))
                     (length buckets))))
    (dolist (pair (svref buckets index) (values default nil))
      (when (hasheq key (car pair))
        (return (values (cdr pair) t))))))

(defun (setf hashref) (new-value hash key)
  "Sets new value for the key in hash table"
  (declare (type hash hash))
  (block hashref
    (let* ((buckets (%hash-buckets hash))
           (code (the ufixnum (hash key)))
           (index (mod code (length buckets))))
      (dolist (pair (svref buckets index))
        (when (hasheq key (car pair))
          (setf (cdr pair) new-value)
          (return-from hashref (values new-value t))))
      (when (> (/ (1+ (float (%hash-elt-count hash)))
                  (float (length buckets)))
               (%hash-threshold hash))
        (hash-grow hash))
      (setf buckets (%hash-buckets hash)
            index (mod code (length buckets)))
      (push (cons key new-value) (svref buckets index))
      (incf (%hash-elt-count hash))
      (values new-value nil))))

(defun hashrem (hash key)
  "Removes key and corresponding value from hash table"
  (declare (type hash hash))
  (let* ((buckets (%hash-buckets hash))
         (code (the ufixnum (hash key)))
         (index (mod code (length buckets))))
    (loop :for b :on (aref buckets index)
          :and prev = nil :then b
          :when (hasheq key (caar b)) :do
            (if prev
              (setf (cdr prev) (cdr b))
              (pop (aref buckets index)))
            (decf (%hash-elt-count hash))
            (return t))))

(defun hashclr (hash)
  "Clears hash table"
  (declare (type hash hash))
  (setf (%hash-buckets hash) (make-array +default-hash-size+
                                         :initial-element nil)
        (%hash-elt-count hash) 0)
  hash)

;;;; vim: ft=lisp et
