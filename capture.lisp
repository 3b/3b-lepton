#++(require '3b-lepton)
(in-package 3b-lepton)
;; non-consing capture stuff.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-frame-size+ (* 160 120))
  (defconstant +max-capture-frames+ (floor most-positive-fixnum
					   +max-frame-size+)))

;;; define various smaller types so we can get optimized calculations
;; type slightly smaller than fixnum for using as ctb-buffer-base
(deftype buffer-base () `(integer 0 ,(- most-positive-fixnum 65536)))
;; type for size of a segment (164 or 244 in lepton 1.x-3.5)
(deftype segment-size-type () '(unsigned-byte 8))
;; index into a single io transaction max (24 - 60 depending on kernel
;; module configuration)
(deftype io-transaction-index-type () '(unsigned-byte 8))
;; type for segment index into IO buffer data (fixnum / segment size)
(deftype segment-index-type () '(unsigned-byte 24))
(deftype capture-frame-size-type () `(integer 0 ,+max-frame-size+))
;; type for index into captured frames (must be small enough to get a
;; fixnum when multiplied by +max-frame-size+ pixels)
(deftype capture-frame-counter-type () `(integer 0 ,+max-capture-frames+))


(defparameter *default-speed* 20000000)
(defun calculate-packet-params (l)
  (flet ((err ()
	   (error "unrecognized configuration ~s?"
		  (list (cols l) (rows l)
			:format (video-format l)
			:telemetry (telemetry l)))))
     ;; 1.x-3.x are 80 pixels per segment in all configurations
    (let ((common '(:pixels 80)))
      (cond
	((and (= (cols l) 80) (= (rows l) 60))
	 ;; lepton 1.x-2.x
	 (cond
	   ((and (not (telemetry l)) (eq (video-format l) :raw14))
	    (list* :length 164 :packets 60 common))
	   ((and (telemetry l) (eq (video-format l) :raw14))
	    (list* :length 164 :packets 63 common))
	   ((and (not (telemetry l)) (eq (video-format l) :raw888))
	    (list* :length 244 :packets 60 common))
	   (t (err))))
	((and (= (cols l) 160) (= (rows l) 120))
	 ;; lepton 3,3.5
	 (cond
	   ((and (not (telemetry l)) (eq (video-format l) :raw14))
	    (list* :length 164 :packets 240 common))
	   ((and (telemetry l) (eq (video-format l) :raw14))
	    (list* :length 164 :packets 244 common))
	   ((and (not (telemetry l)) (eq (video-format l) :raw888))
	    (list* :length 244 :packets 240 common))
	   (t (err))))
	(t (err))))))

(declaim (inline is-discard lost-sync))
(defun is-discard (v o)
  (= #xf (ldb (byte 4 0) (aref v (1+ o)))))


(defun lost-sync (v o size &optional (limit (+ (ash size -1) (ash size -2))))
  ;; heuristic to try to notice loss of sync, check for zero CRC
  ;; or over 75% 0s
  (or (and (zerop (aref v (+ o 2)))
	   (zerop (aref v (+ o 3))))
      (and (not (is-discard v o))
	   (> (the fixnum (count 0 v :start o :end (+ o size)))
	      limit))))

(declaim (inline read-packet))
(defun read-packet (fd ctb index count)
  (cl-spidev-lli::read-chunked* fd ctb index count))

(declaim (inline ctb-offset))
(defun ctb-offset (ctb offset
		   &optional (base (cl-spidev-lli::ctb-buffer-base ctb)))
  (+ (the buffer-base base)
     (* (the io-transaction-index-type offset)
	(the segment-size-type (cl-spidev-lli::ctb-stride ctb)))))

(defun wait-for-sync (spi ctb index)
  (declare (optimize speed)
	   (type segment-index-type index))
  (loop
     with resyncs fixnum  = 0
     with buffer of-type octet-vector = (cl-spidev-lli::ctb-buffer ctb)
     with size of-type segment-size-type = (cl-spidev-lli::ctb-stride ctb)
     with limit of-type segment-size-type = (+ (ash size -1) (ash size -2))
     with fd = (cl-spidev::handle-stream spi)
     ;; fixme: handle timeouts better
     with max = 1000
     with b1 of-type buffer-base = (ctb-offset ctb index)
     for i below max
     for r = (read-packet fd ctb index 1)
     when (lost-sync buffer b1 size limit)
     ;;; TODO: try to implement optional 'fast resync'
     ;; search for ff ff xx 00 00...  then if high bits of xx are all
     ;; 1s, read bits to shift high bits out, then read bytes to get
     ;; to next packet edge based on position of ff bytes. (if not
     ;; found, or xx isn't high 1s, low 0s, do normal resync)
     do (incf resyncs)
     ;; idle device for 5+ frames so spi times out
       (sleep 0.185)
     ;; then try again
       (read-packet fd ctb index 1)
     unless (is-discard buffer b1)
     return resyncs))

(defvar *foo* nil)
(defun read-segment (spi ctb index segment-size max-reads)
  (declare (optimize speed)
	   (type segment-size-type segment-size)
	   (type segment-index-type index)
	   (type io-transaction-index-type max-reads))
  #++(push (list :@ index segment-size max-reads) *foo*)
  (cl-spidev-lli::update-ctb-offsets ctb index)
  #++(push (list :w 0) *foo*)

  (let ((a (wait-for-sync spi ctb 0)))
    (decf segment-size)
    (incf index)
    (loop
       with fd = (cl-spidev::handle-stream spi)
       for i1 = 1 then 0
       for n = (min max-reads segment-size)
       do
	 #++(push (list :s i1 n) *foo*)
	 (read-packet fd ctb i1 n)
	 (incf index n)
	 (decf segment-size n)
       while (plusp segment-size)
       do #++(push (list :@ index) *foo*)
	 (cl-spidev-lli::update-ctb-offsets ctb index))
    a))

(defun read-frame-3 (spi ctb index packets-per-segment max-reads
		     &key filter-segment )
  (declare (optimize speed)
	   (type segment-index-type index packets-per-segment))
  ;; read a lepton 3/3.5 frame:
  ;; wait for sync
  ;; read a segment
  ;; repeat above until we get a valid segment 1
  ;; repeat 3 more times to get full frame
  #++(push (list :f index max-reads) *foo*)
  (loop
     with filter-segment of-type (or null function)
       = (if (and filter-segment (symbolp filter-segment))
	     (fdefinition filter-segment)
	     filter-segment)
     with resyncs fixnum = 0
     with i1 of-type segment-index-type = index
     with segment of-type (unsigned-byte 8) = 1 ;; 1-4
     with buffer of-type octet-vector = (cl-spidev-lli::ctb-buffer ctb)
     ;;for base fixnum = (cl-spidev-lli::ctb-buffer-base ctb)
     do
       (cl-spidev-lli::update-ctb-offsets ctb i1)
       (let* ((offset-20 (ctb-offset ctb 20))
	      (r (read-segment spi ctb i1 packets-per-segment max-reads)))
	 (declare (fixnum r offset-20))
	 (setf resyncs
	       (ldb (byte #.(integer-length most-positive-fixnum) 0)
		    (+ resyncs
		       r)))
	 ;; see if we got the segment we want
	 (if (= (ldb (byte 4 4) (aref buffer offset-20))
		segment)
	     (progn
	       #++
	       (push (list :f+ offset-20
			   (aref buffer offset-20)
			   (aref buffer (+ 1 offset-20)))
		     *foo*)
	       ;; if so, optionally filter it
	       (when filter-segment
		 (funcall filter-segment (1- segment) i1))
	       (setf segment (1+ segment)
		     i1 (+ i1 packets-per-segment)))
	     (progn
	       #++
	       (push (list :fx offset-20
			   (aref buffer offset-20)
			   (aref buffer (+ 1 offset-20)))
		     *foo*)
	       (setf segment 1 ;; wrong or invalid segment, restaart
		    i1 index))))

     while (< segment 5)
     finally (return resyncs)))

(defun sf-raw (ctb in-pointer output-buffer telemetry
	        pixels-per-segment packets-per-segment)
  (declare (type segment-size-type pixels-per-segment)
	   (type (unsigned-byte 8) packets-per-segment)
	   (type capture-frame-size-type))
  (macrolet
      ((body (tx)
	 `(let ((frame 0)
		(segments 0)
		(pixel-offset 0))
	    (declare (type capture-frame-counter-type frame)
		     (type (unsigned-byte 4) segments)
		     (type buffer-base pixel-offset))
	    (lambda (segment index)
	      (declare (type (simple-array (unsigned-byte 16) 1)
			     output-buffer)
		       (optimize speed)
		       (type (integer 0 5) segment)
		       (type segment-index-type index))
	      (loop
		 with len of-type segment-size-type
		   = (cl-spidev-lli::ctb-stride ctb)
		 ;; don't filter header/footer, if any
		 for packet of-type (unsigned-byte 8)
		 from ,(if (eq tx :header)
			   `(if (= segment 0)
				4
				0)
			   0)
		 below ,(if (eq tx :footer)
			    `(if (= segment 3)
				 (- packets-per-segment 4)
				 packets-per-segment)
			    'packets-per-segment)
		 for base of-type buffer-base
		   = (ctb-offset ctb (+ index packet) 0)
		 do
		   (loop
		      for o from (+ 4 base) below (+ base len) by 2
		      ;; o is byte offset, so mem-ref not mem-aref
		      for v of-type word = (cffi:mem-ref in-pointer
							 :unsigned-short o)
		      for i from pixel-offset below (+ pixel-offset len)
		      do (setf (aref output-buffer i)
			       (1+ (3b-i2c::swab16 v))))
		   (incf pixel-offset pixels-per-segment))

	      ;; might lose sync or start in middle of a frame, so
	      ;; be prepared to restart frame
	      (when (= segment 0)
		(setf segments 0))
	      (setf segments (logior segments (ash 1 segment)))
	      ;; if we got 4 segments, move to next frame
	      (when (= segments #xf)
		(setf segments 0)
		(incf frame))))))
    (cond
      ((eq telemetry :header) (body :header))
      ((eq telemetry :footer) (body :footer))
      (t (body nil)))))


(defun capture-frames (lepton &key (count 1)
				;; for now must output 1 value
				(filter-output-type '(unsigned-byte 16))
				(filter-generator 'sf-raw))
  (sleep 0.185)
  (setf (cl-spidev:max-speed (spi lepton)) *default-speed*)
  (update-metadata lepton)
  (check-type count capture-frame-counter-type)
  (let* ((params (calculate-packet-params lepton))
	 (telemetry (and (telemetry-enable lepton)
			 (telemetry-location lepton)))
	 (rows (rows lepton))
	 (cols (cols lepton))
	 (bytes-per-packet (getf params :length))
	 (packets-per-frame (getf params :packets))
	 (packets-per-segment (/ packets-per-frame 4))
	 (pixels-per-segment (getf params :pixels))
	 ;; fixme: detect or make configurable
	 (kernel-buffer-size 4096)
	 ;; max packets per IO with default linux spidev buffer size
	 (max-reads (floor kernel-buffer-size bytes-per-packet))
	 ;; fixme: probably can simplify read-frame to only use 1
	 ;; segment if we always filter/copy out every segment?
	 (io-buffer (make-array (* 2 packets-per-frame bytes-per-packet)
				:element-type '(unsigned-byte 8)
				:initial-element #xb8))
	 ;; enough space for COUNT frames
	 (output-buffer (make-array (* count rows cols)
				    :element-type filter-output-type)))
    (format t "~&~s ~s ~s ~s ~s ~s ~s ~s~%"
	    rows cols bytes-per-packet packets-per-frame packets-per-segment
	    pixels-per-segment kernel-buffer-size max-reads)
    (cl-spidev-lli::with-transfer-buffers (ctb
					   io-buffer
					   bytes-per-packet
					   (+ 2 max-reads)
					   #++ packets-per-segment
					   *default-speed* 0 8)
      (cffi:with-pointer-to-vector-data (io-pointer io-buffer)
	(let ((filter (funcall filter-generator
			       ctb io-pointer
			       output-buffer telemetry
			       pixels-per-segment
			       packets-per-segment)))
	  (print
	   (loop repeat count
	      collect (read-frame-3 (spi lepton) ctb 0
				    packets-per-segment
				    max-reads
				    :filter-segment filter))))
	(format t "~&~x~%" (loop for i below 240
			      collect (subseq io-buffer (* i 164)
					      (+ (* i 164) 24))))))
    output-buffer))

#++
(with-lepton (l)
  (setf *foo* nil)
  (setf (oem-video-source l) :constant)
  (setf (oem-video-output-source-constant l) #x3b13)
  (setf (telemetry-enable l) nil)
  (setf (telemetry-location l) :footer)
  (format t "~x" (loop with f = (capture-frames l :count 1)
		    for i below 15
		    collect (subseq f (* i 80) (+ (* i 80) 24)
				    )))
  (format t "~&~{~a~%~}"  (reverse *foo*)))



#++
(with-lepton (l)
  (reboot l))
