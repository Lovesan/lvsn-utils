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

(defun format-hash (bytes &key (start 0) end)
  (declare (type simple-octet-vector bytes)
           (type index start)
           (type (or null index) end))
  (with-output-to-string (out)
    (loop :for i :from start :below (or end (length bytes))
          :do (format out "~2,'0x" (aref bytes i)))))

(defun md5string (str &key (string t) (external-format :default) (start 0) end)
  (declare (type string str)
           (type index start)
           (type (or null index) end))
  (let ((bytes (sb-md5:md5sum-string str :external-format external-format
                                         :start start
                                         :end end)))
    (if string
      (format-hash bytes)
      bytes)))

(defun md5bytes (sequence &key (string t) (start 0) end)
  (declare (type (vector octet) sequence)
           (type index start)
           (type (or null index) end))
  (let ((bytes (sb-md5:md5sum-sequence sequence :start start :end end)))
    (if string
      (format-hash bytes)
      bytes)))

(defun md5file (filename &key (string t) (start 0) end)
  (declare (type pathname-designator filename)
           (type index start)
           (type (or null index) end))
  (let ((data (file-to-bytes filename :start start :end end)))
    (md5bytes data :string string)))

;;;; vim: ft=lisp et
