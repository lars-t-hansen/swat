;;; -*- fill-column: 80; indent-tabs-mode: nil; show-trailing-whitespace: t -*-
;;;
;;; Copyright 2018 Lars T Hansen.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; This is R7RS Scheme.

(import (scheme base)
	(scheme cxr)
	(scheme char)
	(scheme file)
	(scheme write)
	(scheme process-context))

(define (remove-file fn)
  (guard (exn (else #t))
    (delete-file fn)))

(define (binarize input-filename output-filename)
  (let ((in  (open-input-file input-filename))
	(out (begin
	       (remove-file output-filename)
	       (open-binary-output-file output-filename))))
    (let loop1 ((c (read-char in)))
      (cond ((eof-object? c)
	     (close-input-port in)
	     (close-output-port out))
	    ((char-whitespace? c)
	     (loop1 (read-char in)))
	    ((char-numeric? c)
	     (let loop2 ((cs (list c)) (c (read-char in)))
	       (if (and (char? c) (char-numeric? c))
		   (loop2 (cons c cs) (read-char in))
		   (let ((n (string->number (list->string (reverse cs)))))
		     (if (<= 0 n 255)
			 (write-u8 n out)
			 (error "Value out of range: " n))
		     (loop1 c)))))
	    (else
	     (error "Garbage character in file: " c))))))

(define (main)
  (with-exception-handler
   (lambda (x)
     (display "Error\n")
     (display (error-object-message x))
     (newline)
     (exit #f))
   (lambda ()
     (let ((args (command-line)))
       (if (not (= (length args) 3))
	   (error "Usage: binarize input-file output-file"))
       (binarize (cadr args) (caddr args))))))

(main)
