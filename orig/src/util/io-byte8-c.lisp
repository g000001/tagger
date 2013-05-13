;;;-*- Package: BINARY-IO; Syntax: Common-Lisp; Mode: Lisp -*-

;;; Copyright (c) 1991 by Xerox Corporation

(cl:in-package :binary-io)

;;;; BYTE8-STREAMS -- all binary i/o should (eventually) pass through these
;;;; macros rather than the corresponding generic CL functions.

(defvar *default-byte8-stream-buffer-size* 512)

(ff:defforeign 'open_byte8_stream :return-type :integer 
	       :arguments '(string string)
	       :entry-point (ff:convert-to-lang "open_byte8_stream"))

(defun open-byte8-stream
    (pathname &key (direction :input) if-exists
     &allow-other-keys)
  (let* ((pathname (namestring pathname))
	 (exists-p (probe-file pathname))
	 (if-exists (or if-exists
			(ecase direction
			  (:input nil)
			  (:output :error)
			  (:io :error))))
	 (type (ecase direction
		 (:input (if exists-p
			     "r"
			   (error "~A: non-existent file." pathname)))
		 (:output "w")
		 (:io (ecase if-exists
			(:overwrite "r+")
			((:supercede :new-version) "w+")
			(:error (if exists-p
				    (error "~A already exists." pathname)
				  "w+"))))))
	 (stream (open_byte8_stream pathname type)))
    (when (null stream)
      (error "Unable to open file ~A." pathname))
    stream))
  
(ff:defforeign 'close-byte8-stream :return-type :integer :arguments '(integer)
	       :entry-point (ff:convert-to-lang "fclose"))

(ff:defforeign 'fflush :return-type :integer :arguments '(integer)
	       :entry-point (ff:convert-to-lang "fflush"))

(defun flush-byte8-stream (stream)
  (unless (zerop (fflush stream))
    (error "Error while flushing ~A" stream)))

(defmacro with-open-byte8-stream ((var pathname &rest keys) &body body)
  `(let ((,var (open-byte8-stream ,pathname ,@keys)))
     (unwind-protect
	  (progn ,@body)
       (close-byte8-stream ,var))))

(ff:defforeign 'byte8-stream-length :return-type :integer 
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "byte8_stream_length")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'pad-byte8-stream :return-type :void
	       :arguments '(fixnum integer)
	       :entry-point (ff:convert-to-lang "pad_byte8_stream")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'byte8-read :return-type :fixnum
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "byte8_read")
	       :callback nil :arg-checking nil :call-direct t)
(ff:defforeign 'byte8-write :return-type :fixnum
	       :arguments '(fixnum integer)
	       :entry-point (ff:convert-to-lang "byte8_write")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'get-byte8-stream-position :return-type :integer
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "get_byte8_stream_position")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'set-byte8-stream-position :return-type :integer
	       :arguments '(integer integer)
	       :entry-point (ff:convert-to-lang "set_byte8_stream_position")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'byte16-read :return-type :fixnum
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "byte16_read")
	       :callback nil :arg-checking nil :call-direct t)
(ff:defforeign 'byte16-write :return-type :fixnum
	       :arguments '(fixnum integer)
	       :entry-point (ff:convert-to-lang "byte16_write")
	       :callback nil :arg-checking nil :call-direct t)

(ff:defforeign 'read-byte32 :return-type :integer
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "read_byte32")
	       :callback nil :arg-checking nil :call-direct t)
(ff:defforeign 'write-byte32 :return-type :integer
	       :arguments '(integer integer)
	       :entry-point (ff:convert-to-lang "write_byte32")
	       :callback nil :arg-checking nil :call-direct t)



(ff:defforeign 'int29-read :return-type :integer
	       :arguments '(integer)
	       :entry-point (ff:convert-to-lang "int29_read")
	       :callback nil :arg-checking nil :call-direct t)
(ff:defforeign 'int29-write :return-type :integer
	       :arguments '(integer integer)
	       :entry-point (ff:convert-to-lang "int29_write")
	       :callback nil :arg-checking nil :call-direct t)


