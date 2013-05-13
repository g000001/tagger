;;;-*- Package: BINARY-IO; Syntax: Common-Lisp; Mode: Lisp -*-

;;; Copyright (c) 1993 by Xerox Corporation

(cl:in-package :binary-io)

(defvar *default-byte8-stream-buffer-size* 512)
 
(defmacro open-byte8-stream
    (pathname &rest keys &key (buffer-size *default-byte8-stream-buffer-size*)
     &allow-other-keys)
  #-excl(declare (ignore buffer-size))
  (let ((keys (copy-list keys)))
    (remf keys :buffer-size)
    `(let (#+excl(excl::stream-buffer-size ,buffer-size))
       (open ,pathname :element-type '(unsigned-byte 8) ,@keys))))
 
(defmacro close-byte8-stream (stream)
  `(close ,stream))
 
(defmacro with-open-byte8-stream ((var pathname &rest keys) &body body)
  `(let ((,var (open-byte8-stream ,pathname ,@keys)))
     (unwind-protect
          (progn ,@body)
       (close-byte8-stream ,var))))
 
 
(defmacro get-byte8-stream-position (stream)
;;; Returns the FILE-POSITION of STREAM
#+xerox
;; inline IL:GETFILEPTR.  very grody, but fast.
  `(let ((stream ,stream))
     (funcall
      (il:ffetchfield
       '(il:fdev 76 il:pointer)
       (il:ffetchfield '(stream 10 il:pointer) ,stream))
      ,stream))
#+allegro-v3.1 `(excl::stm-fio-file-position ,stream nil)
#+(and allegro (version>= 4)) `(excl::stm-file-file-position ,stream nil)
#+cmu `(lisp::fd-stream-file-position ,stream nil)
#-(or xerox allegro cmu) `(file-position ,stream))
 
(defmacro set-byte8-stream-position (stream position)
;;; Sets the FILE-POSITION of STREAM to POSITION (in byte8s)
#+xerox `(il:setfileptr ,stream ,position)
#+allegro-v3.1 `(excl::stm-fio-file-position ,stream ,position)
#+(and allegro (version>= 4)) `(excl::stm-file-file-position ,stream ,position)
#+cmu `(lisp::fd-stream-file-position ,stream ,position)
#-(or xerox allegro cmu) `(file-position ,stream ,position))
 
(defmacro byte8-stream-length (stream)
  `(file-length ,stream))
 
(defmacro pad-byte8-stream (n-byte8s stream)
;;; Write N-BYTE8S of zeros on STREAM.  Used to pad out pages.
  `(let ((n-byte8s ,n-byte8s)
         (stream ,stream))
    (declare (type byte16 n-byte8s))
    (dotimes (i n-byte8s)
      (declare (type byte16 i))
      (byte8-write 0 stream))))

(defmacro flush-byte8-stream (stream)
  `(finish-output ,stream))



;;;; BYTE8 i/o: these should be as fast as possible

(defmacro byte8-write (byte8 stream)
#+lucid `(lcl:fast-write-byte ,byte8 ,stream (unsigned-byte 8))
#+xerox (once-only (stream byte8) `(il:\\bout ,stream ,byte8))
#+allegro-v3.1 (once-only (byte8 stream)
		 `(excl::stm-fio-buffered-write-char ,stream ,byte8))
#+(and allegro (version>= 4))
  (once-only (byte8 stream)
    `(excl::stm-file-buffered-write-char ,stream ,byte8))
#+cmu (once-only (byte8 stream)
	`(lisp::output-byte-full-buffered ,stream ,byte8))
#-(or xerox allegro lucid cmu) `(write-byte ,byte8 ,stream))
 
(defmacro byte8-read (stream)
  `(the byte8
	#+allegro (fast-byte8-read ,stream)		   
	#+lucid (lcl:fast-read-byte ,stream (unsigned-byte 8) t nil)
	#+xerox (il:bin ,stream)
	#+cmu (fast-byte8-read ,stream)
	#+mcl (locally (declare (optimize (speed 3) (safety 0)))
		(ccl::%fread-byte (svref ,stream 3)))
	#-(or xerox lucid allegro cmu mcl) (read-byte (the stream ,stream))))

#+cmu
(defun fast-byte8-read (stream)
  (declare (optimize (speed 3) (safety 0)))
  (lisp::input-at-least stream 1)
  (prog1 (lisp::sap-ref-8  (lisp::fd-stream-ibuf-sap stream)
			   (lisp::fd-stream-ibuf-head stream))
    (incf (lisp::fd-stream-ibuf-head stream) 1)))

#+allegro-v3.1
(progn
  (defmacro stream-buffer (stream)
    `(the (simple-array byte8 (*)) (svref ,stream 12)))
  (defmacro stream-buffpos (stream)
    `(the fixnum (svref ,stream 14)))
  (defmacro stream-maxbuffpos (stream)
    `(the fixnum (svref ,stream 15)))
  (defun fast-byte8-read (stream)
    (declare (optimize (speed 3) (safety 0)))
    (let ((buffpos (stream-buffpos stream))
	  (maxbuffpos (stream-maxbuffpos stream)))
      (declare (fixnum buffpos maxbuffpos))
      (if (>= buffpos maxbuffpos)
	  (read-byte stream)
	  (prog1 (aref (stream-buffer stream) buffpos)
	    (when (= (setf (stream-buffpos stream)
			   (the fixnum (1+ buffpos)))
		     maxbuffpos)
	      (setf (stream-maxbuffpos stream) 0)))))))

#+(and allegro (version>= 4))
(defun fast-byte8-read (stream)
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((stream-slots (stream)
	       `(the simple-vector (svref ,stream 1)))
	     (stream-buffer (slots)
	       `(the (simple-array byte8 (*)) (svref ,slots 11)))
	     (stream-buffpos (slots)
	       `(the fixnum (svref ,slots 12)))
	     (stream-maxbuffpos (slots)
	       `(the fixnum (svref ,slots 13))))
    (let* ((slots (stream-slots stream))
	   (buffpos (stream-buffpos slots))
	   (maxbuffpos (stream-maxbuffpos slots)))
      (declare (fixnum buffpos maxbuffpos))
      (if (>= buffpos maxbuffpos)
	  (stream:stream-read-byte stream)
	  (prog1 (aref (stream-buffer slots) buffpos)
	    (when (= (setf (stream-buffpos slots)
			   (the fixnum (1+ buffpos)))
		     maxbuffpos)
	      (setf (stream-maxbuffpos slots) 0)))))))


;;;; BYTE16 and BYTE32 i/o

(declaim (inline byte16-read byte16-write byte16-order))

(defun byte16-write (byte16 stream)
;;; Writes a BYTE16 on STREAM for reading by BYTE16-READ
  (declare (type byte16 byte16) #.tdb:*highly-optimized*)
  (byte8-write (ldb (byte 8 8) byte16) stream)
  (byte8-write (ldb (byte 8 0) byte16) stream))

(declaim (function byte16-read (stream) byte16))
(defun byte16-read (stream)
;;; reads & returns a byte16 from STREAM as written by BYTE16-WRITE
  (declare #.tdb:*highly-optimized*)
  (the byte16
       (logior (the byte16 (ash (the byte8 (byte8-read stream)) 8))
	       (byte8-read stream))))


(defun write-byte32 (byte32 stream)
  (declare (type byte32 byte32) #.tdb:*highly-optimized*)
  (if (typep byte32 'fixnum)
      (progn
	(byte16-write (ash (the fixnum byte32) -16) stream)
	(byte16-write (logand (the fixnum byte32) #xffff) stream))
    (progn (byte16-write (ash byte32 -16) stream)
	   (byte16-write (logand byte32 #xffff) stream))))

(defun read-byte32 (stream)
  (declare #.tdb:*highly-optimized*)
  (let ((hi16 (byte16-read stream))
	(lo16 (byte16-read stream)))
    (declare (type byte16 hi16 lo16))	; optimize for tagged 32 architectures
    (if (< hi16 (ash most-positive-fixnum -16))
	(the (or byte16 fixnum)
	  (logior (the (or byte16 fixnum) (ash hi16 16)) lo16))
      (the byte32 (+ (the byte32 (ash hi16 16)) lo16)))))




;;;; variable length, signed integers up to 29 bits (28+sign).
;;;; representation optimized for positive numbers (byte28s)

;;; Use high order bit of each byte to indicate whether more bytes are present.
;;; Low order bytes are written first.  A high order byte of zero indicates a
;;; negative number.  Don't allow for more than 28 bits of magnitude -- this is
;;; both enough for our purposes and keeps things in the fixum range. 


(defun int29-write (int29 stream)
;;; write INT29 on STREAM
  (declare #.tdb:*highly-optimized*)
; (unless (typep int29 'int29) (error "~D is not an INT29!" int29))
  (let* ((int29 int29)
	 (magnitude (if (minusp int29) (- 0 int29) int29))
	 (byte7 0))
    (declare (type int29 int29) (type byte28 magnitude) (type byte7 byte7))
    (loop
     (setq byte7 (logand magnitude 127))
     (setq magnitude (ash magnitude -7))
     (cond
       ((zerop magnitude)
	(cond
	  ((minusp int29)
	   (byte8-write (logior byte7 128) stream)
	   (byte8-write 0 stream))
	  (t (byte8-write byte7 stream)))
	(return))
       (t (byte8-write (logior byte7 128) stream))))))

(defun int29-read (stream)
  (declare #.tdb:*highly-optimized*)
  (let* ((byte8 (byte8-read stream))
         (byte28 (logand byte8 127))
         (shift 7))
    (declare (type (unsigned-byte 8) byte8)
             (type byte28 byte28)
             (type byte7 shift))
    (loop
     (when (zerop (logand byte8 128))
       (return byte28))
     (setq byte8 (byte8-read stream))
     (when (zerop byte8)
       (return (the int29 (- 0 byte28))))
     (setq byte28 (logior (the byte28 (ash (logand byte8 127) shift)) byte28))
     (incf shift 7))))
