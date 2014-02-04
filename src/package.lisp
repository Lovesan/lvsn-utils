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

(in-package #:cl-user)

(defpackage #:lvsn-utils
  (:nicknames #:lvsn)
  (:use #:cl)
  (:export
   #:defhelper
   #:with-gensyms
   #:mklist
   #:mklistf
   #:mkstring
   #:mkstringf
   #:mkgensym
   #:mkgensymf
   #:with-gensyms
   #:with-collect
   #:collect
   #:do-map-reduce
   #:dolist*
   #:doseq
   #:doseq*

   #:index
   #:pindex
   #:ufixnum
   #:pathname-designator
   #:simple-character-string
   #:simple-char-string
   #:character-string
   #:char-string
   #:octet
   #:octet-vector
   #:simple-octet-vector
   #:make-octet-vector
   #:make-octet-vector*
   #:make-char-string
   #:make-char-string*

   #:hash
   #:make-hash
   #:hasheq
   #:hashref
   #:hashrem
   #:hashclr

   #:use-package*
   #:special-variable-p
   #:symbol-macro-p
   #:symbol-attributes
   #:list-external-symbols
   #:external-symbol-p
   #:apropos-all
   #:apropos-package

   #:md5string
   #:md5bytes
   #:md5file
   #:format-hash

   #:mkpathname
   #:mkpathnamef
   #:dirpath
   #:dirpathf
   #:dirpathp
   #:dirname
   #:dir
   #:cwd
   #:cd
   #:dirwalk
   #:with-output-to-nil
   #:with-stdout-to-string
   #:with-stdout-to-nil
   #:with-stderr-to-string
   #:with-stderr-to-nil
   #:with-new-file
   #:with-append-file
   #:file-to-bytes
   #:file-to-string
   #:file-to-lines
   #:string-to-lines
   #:lines-to-string
   #:bytes-to-file
   #:string-to-file
   #:lines-to-file
   #:grep
   ))

;;;; vim: ft=lisp et
