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

(defhelper mkpathname (path)
  (declare (type pathname-designator path))
  (let ((path (pathname path)))
    ;; workaround for strange sbcl behavior
    (if (and (string= "." (pathname-name path))
             (string= "" (pathname-type path)))
      (pathname (format nil "~a/" path))
      path)))

(define-modify-macro mkpathnamef () mkpathname)

(defun dirpath (path)
  (declare (type pathname-designator path))
  (mkpathnamef path)
  (if (pathname-name path)
    (make-pathname :defaults path
                   :name nil
                   :type nil
                   :directory (if (endp (pathname-directory path))
                                (list :relative (pathname-name path))
                                (append (pathname-directory path)
                                        (list (pathname-name path)))))
    (make-pathname :defaults path
                   :name nil
                   :type nil)))

(define-modify-macro dirpathf () dirpath)

(defun dirpathp (path)
  (declare (type pathname-designator path))
  (mkpathnamef path)
  (and (null (pathname-name path))
       (null (pathname-type path))))

(defun dirname (path)
  (declare (type pathname-designator path))
  (mkpathnamef path)
  (if (dirpathp path)
    (make-pathname :defaults path
                   :directory (butlast (pathname-directory path)))
    (make-pathname :defaults path
                   :name nil
                   :type nil)))

(defun dir (path &key (name :wild) (type :wild) (files t) (directories t))
  (declare (type pathname-designator path))
  (let ((entries (directory (make-pathname :defaults (dirpath path)
                                           :name name
                                           :type type))))
    (unless directories
      (setf entries (delete-if #'dirpathp entries)))
    (unless files
      (setf entries (delete-if (complement #'dirpathp) entries)))
    entries))

(defun cwd ()
  *default-pathname-defaults*)

(defun cd (path)
  (declare (type pathname-designator path))
  (setf *default-pathname-defaults* (truename (dirpath path))))

(defun dirwalk (function &key (path (cwd))
                              (pattern "*.*")
                              (files t)
                              (directories nil))
  (declare (type pathname-designator path pattern))
  (dirpathf path)
  (mkpathnamef pattern)
  (let ((name (pathname-name pattern))
        (type (pathname-type pattern)))
    (labels ((walk (dir)
               (dolist (p (dir dir :name name
                                   :type type
                                   :files files))
                 (when (or (not (dirpathp p))
                           directories)
                   (funcall function p)))
               (dolist (d (dir dir :files nil))
                 (walk d))))
      (walk path))))

(defmacro with-output-to-nil ((var) &body body)
  `(with-open-stream (,var (make-broadcast-stream))
     ,@body))

(defmacro with-stdout-to-string (&body body)
  `(with-output-to-string (*standard-output*)
     ,@body))

(defmacro with-stdout-to-nil (&body body)
  `(with-output-to-nil (*standard-output*)
     ,@body))

(defmacro with-stderr-to-string (&body body)
  `(with-output-to-string (*standard-error*)
     ,@body))

(defmacro with-stderr-to-nil (&body body)
  `(with-output-to-nil (*standard-error*)
     ,@body))

(defmacro with-new-file ((var name &key (external-format :default)
                                        (element-type ''character))
                         &body body)
  `(with-open-file (,var ,name :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format ,external-format
                               :element-type ,element-type)
     ,@body))

(defmacro with-append-file ((var name &key (external-format :default)
                                           (element-type ''character))
                            &body body)
  `(with-open-file (,var ,name :direction :output
                               :if-exists :append
                               :if-does-not-exist :create
                               :external-format ,external-format
                               :element-type ,element-type)
     ,@body))

(defun file-to-bytes (filename &key (start 0) end)
  (declare (type pathname-designator filename)
           (type index start)
           (type (or null index) end))
  (with-open-file (in filename :element-type 'octet)
    (let* ((end (or end (file-length in)))
           (buffer (make-octet-vector (- end start))))
      (file-position in start)
      (values buffer (read-sequence buffer in)))))

(defun file-to-string (filename &key (start 0)
                                     end
                                     (buffer-size 4096)
                                     (external-format :default))
  (declare (type pathname-designator filename)
           (type index start)
           (type (or null index) end)
           (type pindex buffer-size buffer-size))
  (let* ((buffer (make-array buffer-size :element-type 'character))
         (end (or end array-total-size-limit)))
    (with-output-to-string (out)
      (with-open-file (in filename
                          :element-type 'character
                          :external-format external-format)
        (dotimes (i start)
          (read-char in nil nil))
        (loop :for n :of-type index = (read-sequence buffer in)
              :while (and (> n 0)
                          (> end start))
              :do (incf start n)
                  (write-string buffer out :end n))))))


(defun string-to-lines (string &key (start 0) end (trim t))
  (declare (type string string))
  (split-sequence:split-sequence
   #\newline
   string
   :start start
   :end end
   :remove-empty-subseqs trim))

(defun file-to-lines (filename &key (start 0)
                                    end
                                    (buffer-size 4096)
                                    (external-format :default)
                                    (trim t))
  (declare (type pathname-designator filename)
           (type index start)
           (type (or null index) end)
           (type pindex buffer-size buffer-size))
  (string-to-lines
   (file-to-string filename :start start
                            :end end
                            :buffer-size buffer-size
                            :external-format external-format)
   :trim trim))

(defun lines-to-string (lines)
  (declare (type list lines))
  (format nil "~{~a~^~%~}" lines))

(defun bytes-to-file (bytes file &key (append t)
                                      (start 0)
                                      end)
  (declare (type (vector octet) bytes)
           (type (or stream string pathname) file)
           (type index start)
           (type (or null index) end))
  (if (streamp file)
    (progn
      (unless append (file-position file 0))
      (write-sequence bytes file :start start :end end))
    (with-open-file (out file :direction :output
                              :if-exists (if append
                                           :append
                                           :supersede)
                              :if-does-not-exist :create
                              :element-type 'octet)
      (write-sequence bytes out :start start :end end)))
  file)

(defun string-to-file (string file &key (external-format :latin-1)
                                        (append t)
                                        (start 0)
                                        end)
  (declare (type string string)
           (type (or stream string pathname) file)
           (type index start)
           (type (or null index) end))
  (if (streamp file)
    (progn
      (unless append (file-position file 0))
      (write-sequence string file :start start :end end))
    (with-open-file (out file :direction :output
                              :if-exists (if append
                                           :append
                                           :supersede)
                              :if-does-not-exist :create
                              :external-format external-format
                              :element-type 'character)
      (write-sequence string out :start start :end end)))
  file)

(defun lines-to-file (lines file &key (external-format :latin-1)
                                      (append t))
  (declare (type list lines)
           (type (or stream string pathname) file))
  (if (streamp file)
    (progn
      (if append
        (fresh-line file)
        (file-position file 0))
      (format file "~{~a~^~%~}" lines))
    (with-open-file (out file :direction :output
                              :if-exists (if append
                                           :append
                                           :supersede)
                              :if-does-not-exist :create
                              :external-format external-format
                              :element-type 'character)
      (when append (fresh-line out))
      (format out "~{~a~^~%~}" lines)))
  file)

(defun grep (pattern path &key (external-format :latin-1)
                               (case-sensitive t)
                               (extended t)
                               (collect nil)
                               (recursive t)
                               (trim t))
  (declare (type pathname-designator path))
  (mkpathnamef path)
  (let ((scanner (cl-ppcre:create-scanner
                  pattern
                  :case-insensitive-mode (not case-sensitive)
                  :extended-mode extended
                  :single-line-mode t))
        (lines '())
        (n 0))
    (declare (type unsigned-byte n))
    (flet ((fun (filename)
             (loop :for line :in (file-to-lines
                                  filename
                                  :external-format external-format
                                  :trim trim)
                   :for i :of-type index :from 1
                   :when (cl-ppcre:scan scanner line) :do
                     (if collect
                       (push (list filename i line) lines)
                       (format t "~a:~a:~a~%"
                               (enough-namestring filename) i line))
                     (incf n))))
      (if recursive
        (dirwalk #'fun :path (dirname path)
                       :pattern (make-pathname :name (pathname-name path)
                                               :type (pathname-type path)))
        (dolist (f (dir (dirname path) :name (pathname-name path)
                                       :type (pathname-type path)
                                       :directories nil))
          (fun f)))
      (values lines n))))

;;;; vim: ft=lisp et
