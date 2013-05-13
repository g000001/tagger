;;;-*- Package: BINARY-IO; Syntax: Common-Lisp; Mode: Lisp -*-

;;; Copyright (c) 1991 by Xerox Corporation

(cl:in-package :binary-io)

(defun byte16-order (byte16-1 byte16-2)
  (declare (type byte16 byte16-1 byte16-2) #.tdb:*highly-optimized*)
  (if (= byte16-1 byte16-2)
      :equal
      (< byte16-1 byte16-2)))

(define-io-fns byte8
    :write-fn (lambda (byte8 stream) (byte8-write byte8 stream))
    :read-fn (lambda (stream) (byte8-read stream))
    :size-fn (lambda (byte8) (check-type byte8 byte8) 1))

(define-io-fns byte16
    :write-fn byte16-write
    :read-fn byte16-read
    :size-fn (lambda (byte16) (check-type byte16 byte16) 2))

(define-io-fns byte32
    :write-fn write-byte32
    :read-fn read-byte32
    :size-fn (lambda (byte32) (check-type byte32 byte32) 4))


;;; half-word swapped versions for back-compatibility

(defun byte32-write (byte32 stream)
;;; the byte ordering is a little funky to be compatible w/ Cedar & Smalltalk
  (declare (type byte32 byte32) #.tdb:*highly-optimized*)
  (byte16-write (ldb (byte 16 0) byte32) stream)
  (byte16-write (ldb (byte 16 16) byte32) stream))

(defun byte32-read (stream)
;;; the byte ordering is a little funky to be compatible w/ Cedar & Smalltalk
  (declare #.tdb:*highly-optimized*)
  (logior (the byte16 (byte16-read stream))
	  (the byte32 (ash (byte16-read stream) 16))))

(deftype swapped-byte32 () 'byte32)

(define-io-fns swapped-byte32
    :write-fn byte32-write
    :read-fn byte32-read
    :size-fn (lambda (byte32) (check-type byte32 byte32) 4))



;;;; variable length, signed integers -- arbitrarily large

(defun integer-write (integer stream)
;;; Write INTEGER to STREAM for reading by INTEGER-READ.
  (declare (integer integer) #.tdb:*highly-optimized*)
  (let* ((minusp (minusp integer))
	 (magnitude (if minusp (- integer) integer))
	 (n-bytes (ceiling (integer-length magnitude) 8)))
    (declare (type byte8 magnitude n-bytes))
    ;; write code byte -- even for positive, odd for negative
    (byte8-write (if minusp (1+ (* n-bytes 2)) (* n-bytes 2)) stream)
    ;; write magnitude -- high order bytes first
    (do ((shift (* (1- n-bytes) -8) (+ shift 8)))
	((plusp shift))
      (declare (type (signed-byte 11) shift))
      (byte8-write (logand #xFF (ash magnitude shift)) stream))))

(defun integer-read (stream)
;;; Read an integer from STREAM as written by INTEGER-WRITE.
  (declare #.tdb:*highly-optimized*)
  (let* ((code-byte (byte8-read stream))
	 (minusp (oddp code-byte))
	 (magnitude 0)
	 (n-bytes (ash code-byte -1)))
    (declare (type byte8 code-byte n-bytes))
    (dotimes (i n-bytes)
      (declare (type byte8 i))
      (setq magnitude (+ (ash magnitude 8) (byte8-read stream))))
    (if minusp (- magnitude) magnitude)))

(defun integer-size (integer)
  (let ((n-bytes (ceiling (integer-length (abs integer)) 8)))
    (unless (< n-bytes (expt 2 8))
      (error "Integer too large: ~D" integer))
    (1+ n-bytes)))

(defun integer-order (i j)
  (declare (integer i j) #.tdb:*highly-optimized*)
  (if (= i j) :equal (< i j)))

(define-io-fns integer
    :write-fn integer-write
    :read-fn integer-read
    :size-fn integer-size
    :order-fn integer-order)



;;;; variable length, signed integers up to 29 bits (28+sign).
;;;; representation optimized for positive numbers (byte28s)

;;; Use high order bit of each byte to indicate whether more bytes are present.
;;; Low order bytes are written first.  A high order byte of zero indicates a
;;; negative number.  Don't allow for more than 28 bits of magnitude -- this is
;;; both enough for our purposes and keeps things in the fixum range. 

(declaim (inline int29-order))

(defun int29-order (int29-1 int29-2)
  (declare (type int29 int29-1 int29-2) #.tdb:*highly-optimized*)
  (if (= int29-1 int29-2)
      :equal
      (< int29-1 int29-2)))

(defun int29-size (int29)
  (declare (type int29 int29) #.tdb:*highly-optimized*)
; (check-type int29 int29)
  (let ((magnitude (if (minusp int29) (- 0 int29) int29))
	(size (if (minusp int29) 2 1)))
    (declare (type byte28 magnitude) (type byte7 size))
    (loop
     (setq magnitude (ash magnitude -7))
     (when (zerop magnitude)
       (return size))
     (incf size))))

(define-io-fns int29
  :write-fn int29-write
  :read-fn int29-read
  :size-fn int29-size
  :order-fn int29-order)



;;;; STRING i/o

(defun string-write (string stream)
;;; Write STRING to STREAM, for reading by STRING-READ.
;;; Strings longer than 2^8 cannot be written by this function.  We pad to 16
;;; bit boundaries for compatibility with Cedar & Smalltalk strings.
;;; We assume STRING is a SIMPLE-STRING, as it has been copied by STRING-COPY.
  (declare (simple-string string) #.tdb:*highly-optimized*)
  (let ((length (length string)))
    (declare (type byte8 length))
    (byte8-write length stream)
    (dotimes (i length)
      (declare (type byte8 i))
      (byte8-write (char-code (schar string i)) stream))
    (if (evenp length) (byte8-write 0 stream)))) ; write pad byte

(defun string-read (stream)
;;; Read & return a string as written by STRING-WRITE.
;;; Always returns a simple string.
  (declare #.tdb:*highly-optimized*)
  (let* ((length (byte8-read stream))
	 (string (alloc-string length)))
    (declare (type byte8 length)
	     (simple-string string))
    (dotimes (i length)			; read string chars
      (declare (type byte8 i))
      (setf (schar string i) (code-char (byte8-read stream))))
    (if (evenp length) (byte8-read stream)) ; read pad byte
    string))


(defun string-size (string)
;;; Returns the number of byte8s STRING-WRITE would use to print STRING.
;;; Does not assume STRING is a SIMPLE-STRING.
  (check-type string string)
  (let ((length (length string)))
    (declare (fixnum length))
    (when (> length +max-string-length+)
      (error "Attempt to use string of length ~D.  Limit is ~D"
	     length +max-string-length+))
    (the byte16 (1+ (logior length 1)))))

(defun simple-string-size (string)
;;; Like STRING-SIZE, but only for SIMPLE-STRINGS.
  (check-type string simple-string)
  (let ((length (length (the simple-string string))))
    (declare (fixnum length))
    (when (> length +max-string-length+)
      (error "Attempt to use string of length ~D.  Limit is ~D"
	     length +max-string-length+))
    (the byte16 (1+ (logior length 1)))))


(defun string-order (string-1 string-2)
;;; case sensitive order function for strings
  (declare #.tdb:*highly-optimized*)
  (let ((mismatch (string/= string-1 string-2)))
    (if mismatch
	(string< string-1 string-2 :start1 mismatch :start2 mismatch)
	:equal)))


(defun stringp-order (string-1 string-2)
;;; case insensitive order function for strings
  (declare #.tdb:*highly-optimized*)
  (let ((mismatch (string-not-equal string-1 string-2)))
    (if mismatch
	(string-lessp string-1 string-2 :start1 mismatch :start2 mismatch)
	:equal)))

#+(and allegro big-endian)
;; lie to the compiler to get word compares
(defun simple-string-order (s1 s2)
  (declare (simple-vector s1 s2) (optimize (speed 3) (safety 0)))
  (let* ((l1 (length s1))
	 (l2 (length s2))
	 (min (min l1 l2)))
    (declare (fixnum l1 l2 min))
    (dotimes (i (ash min -2)
	       (do ((j (logandc2 min #b11) (1+ j)))
		   ((= j min) (if (= l1 l2) :equal (< l1 l2)))
		 (declare (fixnum j))
		 (let ((c1 (schar s1 j))
		       (c2 (schar s2 j)))
		   (when (char/= c1 c2) (return (and (char< c1 c2) t))))))
      (declare (fixnum i))
      (let ((w1 (aref s1 i))
	    (w2 (aref s2 i)))
	(declare (fixnum w1 w2))
	(when (/= w1 w2) (return (< w1 w2)))))))
#-(and allegro big-endian)
(defun simple-string-order (s1 s2)
  (declare (simple-string s1 s2) #.tdb:*highly-optimized*)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (declare (fixnum l1 l2))
    (dotimes (i (min l1 l2) (if (= l1 l2) :equal (and (< l1 l2) t)))
      (declare (fixnum i))
      (let ((c1 (schar s1 i))
            (c2 (schar s2 i)))
	(when (char/= c1 c2)
	  (return (and (char< c1 c2) t)))))))

(defvar *lower-case* (make-string char-code-limit))
(dotimes (i char-code-limit)
  (setf (schar *lower-case* i) (char-downcase (code-char i))))

(defun simple-stringp-order (s1 s2)
  (declare (simple-string s1 s2) #.tdb:*highly-optimized*)
  (let ((l1 (length s1))
	(l2 (length s2)))
    (declare (fixnum l1 l2))
    (dotimes (i (min l1 l2) (if (= l1 l2) :equal (and (< l1 l2) t)))
      (declare (fixnum i))
      (let ((c1 (schar *lower-case* (char-code (schar s1 i))))
	    (c2 (schar *lower-case* (char-code (schar s2 i)))))
	(when (char/= c1 c2)
	  (return (and (char< c1 c2) t)))))))



;;; Note: internally we always have simple strings, but users may pass in
;;; non-simple strings (unless SIMPLE-STRING is specified) which we copy.

(define-io-fns string
    :write-fn string-write
    :read-fn string-read
    :size-fn string-size
    :order-fn string-order
    :free-fn free-string
    :copy-fn string-copy)

(define-io-fns stringp
    :write-fn string-write
    :read-fn string-read
    :size-fn string-size
    :order-fn stringp-order
    :free-fn free-string
    :copy-fn string-copy)

(define-io-fns simple-string
    :write-fn string-write
    :read-fn string-read
    :size-fn simple-string-size
    :order-fn simple-string-order
    :free-fn free-string
    :copy-fn simple-string-copy)

(define-io-fns simple-stringp
    :write-fn string-write
    :read-fn string-read
    :size-fn simple-string-size
    :order-fn simple-stringp-order
    :free-fn free-string
    :copy-fn simple-string-copy)



;;;; SINGLE-FLOAT i/o

;;; This code should work as-is in implementations in which SINGLE-FLOAT
;;; corresponds to IEEE single-precision.

#-(or ieee-floating-point mcl)
(progn
  (warn "IEEE-FLOATING-POINT not present in *FEATURES*.")
  (warn "SINGLE-FLOAT output not guaranteed valid."))

(defun single-float-write (single-float stream)
;;; Write SINGLE-FLOAT on STREAM in IEEE single-precision float format
  (declare (type single-float single-float))
  #+xerox
  (progn (byte16-write (il:\\getbase single-float 0) stream)
	 (byte16-write (il:\\getbase single-float 1) stream))
  #-xerox
  (multiple-value-bind (signif expon sign)
      (integer-decode-float
       ;; in Lucid and MCL single-floats are IEEE double-precision
       #+lucid (lcl:round-to-single-precision single-float)
       #+mcl (float single-float 0.0s0)
       #-(or lucid mcl) single-float)

    (declare (type (unsigned-byte 24) signif)
	     (type (signed-byte 9) expon)
	     (type (signed-byte 2) sign))

    (when (< signif (expt 2 23))
      (setq expon (- (+ 127 23))))

    (byte16-write
     (logior (the (unsigned-byte 16) (ash (if (minusp sign) 1 0) 15))
	     (the (unsigned-byte 15) (ash (the byte8 (+ expon 127 23)) 7))
	     (ldb (byte 7 16) signif))
     stream)
    (byte16-write (ldb (byte 16 0) signif) stream)))

(defun single-float-read (stream)
;;; read an IEEE single-precision float from STREAM
  #+xerox
  (cl::%float (byte16-read stream) (byte16-read stream))
  #-xerox
  (let* ((hi-16 (byte16-read stream))
	 (sign (ldb (byte 1 15) hi-16))
	 (exponent (ldb (byte 8 7) hi-16))
	 (lo-16 (byte16-read stream))
	 (signif (logior (if (= exponent 0) 0 (ash 1 23))
			 (logior
			  (the (unsigned-byte 23)
			    (ash (ldb (byte 7 0) hi-16) 16))
			   lo-16)))
	 (float (if (zerop exponent)
		    (if (zerop signif)
			0.0
			(scale-float (float signif) (- exponent 126 23)))
		    (scale-float (float signif) (- exponent 127 23)))))
    (declare (type byte16 hi-16 lo-16)
	     (type (unsigned-byte 24) signif)
	     (type (signed-byte 8) exponent)
	     (type (unsigned-byte 1) sign)
	     (type single-float float))
    (if (zerop sign) float (- float))))

(defun single-float-size (single-float)
  (check-type single-float single-float)
  4)

(defun single-float-order (float-1 float-2)
  (declare (single-float float-1 float-2))
  (if (= float-1 float-2)
      :equal
      (< float-1 float-2)))

(define-io-fns single-float 
    :write-fn single-float-write
    :read-fn single-float-read
    :size-fn single-float-size
    :order-fn single-float-order)




;;;; Diagnostics

(defun debug-io-fns (name)
;;; Sets things up so that the binary-io named NAME will check entry sizes
;;; against its size fn every time something is read or written.
;;; To turn this checking off, call (UNDEBUG-IO-FNS NAME).
  (unless (gethash name *fns-being-debugged*)
    (let* ((old-io (find-io-fns name))
	   (new-io
	    (typecase old-io
	      (ordered-io-fns (copy-ordered-io-fns old-io))
	      (io-fns (copy-io-fns old-io))
	      (t (error "~A not a defined for BINARY-IO" name)))))
      (setf (io-read-fn new-io)
       #'(lambda (stream)
	   (let* ((pos (get-byte8-stream-position stream))
		  (object (funcall (io-read-fn old-io) stream))
		  (bytes-read (- (get-byte8-stream-position stream) pos))
		  (size (funcall (io-size-fn old-io) object)))
	     (unless (= bytes-read size)
	       (error
		"READ-FN read ~D bytes but SIZE-FN returns ~D for ~S"
		bytes-read size object))
	     object)))
      (setf (io-write-fn new-io)
       #'(lambda (object stream)
	   (let* ((pos (get-byte8-stream-position stream))
		  (bytes-written
		   (progn
		     (funcall (io-write-fn old-io) object stream)
		     (- (get-byte8-stream-position stream) pos)))
		  (size (funcall (io-size-fn old-io) object)))
	     (unless (= bytes-written size)
	       (error
		"WRITE-FN wrote ~D bytes but SIZE-FN returns ~D for ~S"
		bytes-written size object)))))
      (setf (gethash name *fns-being-debugged*) old-io)
      (setf (find-io-fns name) new-io)
      name)))


(defun undebug-io-fns (&optional (name nil name-specified))
;;; If NAME is specified then turns off debugging on the io so named,
;;; otherwise all debugged fns are returned to normal.
  (if name-specified
      (when (gethash name *fns-being-debugged*)
	(setf (find-io-fns name)
	      (prog1 (gethash name *fns-being-debugged*)
		(remhash name *fns-being-debugged*)))
	name)
      (let ((names ()))
	(maphash			; collect names of debugged
	 #'(lambda (name io)
	     (declare (ignore io))
	     (push name names))
	 *fns-being-debugged*)
	(dolist (name names)		; undebug them
	  (undebug-io-fns name))
	names)))			; return list of names
