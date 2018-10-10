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
;; type for pixel index in a multiple sample per pixel buffer
(deftype pixel-index-type (samples) `(unsigned-byte
                                      ,(- (integer-length most-positive-fixnum)
                                          (integer-length (1- samples)))))

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
                     output-buffer
                     &key filter-segment)
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
                 (funcall filter-segment output-buffer (1- segment) i1))
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

(defmacro define-pixel-filter (name (var &key (element-type
                                               '(unsigned-byte 16))
                                         (samples 1))
                               &body body)
  `(defun ,name (ctb in-pointer telemetry pixels-per-frame
                 pixels-per-packet packets-per-segment)
     (declare (type segment-size-type pixels-per-packet)
              (type (unsigned-byte 8) packets-per-segment)
              (type capture-frame-size-type))
     (macrolet
         ((body (tx)
            `(let ((frame 0)
                   (segments 0))
               (declare (type capture-frame-counter-type frame)
                        (type (unsigned-byte 4) segments))
               (lambda (output-buffer segment index)
                 (declare (type (simple-array ,',element-type 1)
                                output-buffer)
                          (optimize speed)
                          (type (integer 0 5) segment)
                          (type segment-index-type index))
                 (loop
                    with pixel-offset of-type buffer-base
                      = (+ (* frame pixels-per-frame)
                           (* segment (* pixels-per-packet
                                         packets-per-segment)))
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
                         for ,',var of-type word
                           = (3b-i2c::swab16
                              (cffi:mem-ref in-pointer :unsigned-short o))
                         for i of-type (pixel-index-type ,',samples)
                         from pixel-offset below (+ pixel-offset len)
                         do (setf (values
                                   ,@ (loop for j below ,samples
                                         collect
                                           `(aref output-buffer
                                                  (+ ,j (* ,',samples i)))) )
                                  (progn ,@',body)))
                      (incf pixel-offset pixels-per-packet))

                 ;; might lose sync or start in middle of a frame, so
                 ;; be prepared to restart frame
                 (when (= segment 0)
                   (setf segments 0))
                 (setf segments (logior segments (ash 1 segment)))
                 ;; if we got 4 segments, move to next frame
                 (when (= segments #xf)
                   (setf segments 0)
                   (incf frame))))))
       (values
        (cond
          ((eq telemetry :header) (body :header))
          ((eq telemetry :footer) (body :footer))
          (t (body nil)))
        ',element-type
        ',samples))))

(declaim (type (simple-array (unsigned-byte 32) 1) *rgb-lut*))
(defvar *rgb-lut*
  ;; default more-or-less grayscale lut with some variation in low bits
  (coerce (loop for i below 16384
             for l = (ldb (byte 8 6) i)
             for r = (min 255 (max 0 (+ l (- (ldb (byte 2 0) i) 2))))
             for g = (min 255 (max 0 (+ l (- (ldb (byte 2 2) i) 2))))
             for b = (min 255 (max 0 (+ l (- (ldb (byte 2 4) i) 2))))
             collect (logior (ash r 16) (ash g 8) b))
          '(simple-array (unsigned-byte 32) 1)))

(define-pixel-filter sf-raw (v :element-type (unsigned-byte 16) :samples 1)
  v)

(define-pixel-filter sf-rgb-lut (v :element-type (unsigned-byte 8) :samples 4)
  (let ((c (aref *rgb-lut* (ash v -2))))
    (values 255 (ldb (byte 8 16) c) (ldb (byte 8 8) c) (ldb (byte 8 0) c))))

(defun capture-frames (lepton &key (count 1)
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
         (pixels-per-packet (getf params :pixels))
         ;; fixme: detect or make configurable
         (kernel-buffer-size 4096)
         (resyncs 0)
         ;; max packets per IO with default linux spidev buffer size
         (max-reads (floor kernel-buffer-size bytes-per-packet))
         ;; fixme: probably can simplify read-frame to only use 1
         ;; segment if we always filter/copy out every segment?
         (io-buffer (make-array (* 2 packets-per-frame bytes-per-packet)
                                :element-type '(unsigned-byte 8)
                                :initial-element #xb8)))
    (cl-spidev-lli::with-transfer-buffers (ctb
                                           io-buffer
                                           bytes-per-packet
                                           (+ 2 max-reads)
                                           #++ packets-per-segment
                                           *default-speed* 0 8)
      (cffi:with-pointer-to-vector-data (io-pointer io-buffer)
        (multiple-value-bind (filter output-type samples)
            (funcall filter-generator
                     ctb io-pointer
                     telemetry
                     (* rows cols)
                     pixels-per-packet
                     packets-per-segment)
          ;; enough space for COUNT frames
          (let ((output-buffer (make-array (* count rows cols samples)
                                           :element-type output-type)))
            (setf resyncs
                  (loop repeat count
                     collect (read-frame-3 (spi lepton) ctb 0
                                           packets-per-segment
                                           max-reads
                                           output-buffer
                                           :filter-segment filter)))
            (values output-buffer resyncs)))))))

#++
(with-lepton (l)
  (setf *foo* nil)
  ;; set camera to return constant data for testing
  (setf (oem-video-source l) :constant)
  (setf (oem-video-output-source-constant l) #x3b13)
  (setf (telemetry-enable l) nil)
  (setf (telemetry-location l) :footer)
  ;; capture a single frame as (unsigned-byte 16) and print upper left corner
  (format t "~x" (loop with f = (capture-frames l :count 1)
                    for i below 15
                    collect (subseq f (* i 80) (+ (* i 80) 24)
                                    )))
  (format t "~&~{~a~%~}" (reverse *foo*)))

#++
(with-lepton (l)
  ;; set camera to return normally processed image data
  (setf (oem-video-source l) :cooked)
  (setf (telemetry-enable l) nil)
  ;; generate a (very bad) color LUT
  (let ((*rgb-lut* (coerce (loop for i below 65535
                              for r = (ldb (byte 8 6) i)
                              for g = (ldb (byte 8 3) i)
                              for b = (ldb (byte 8 0) i)
                              collect (logior (ash r 16) (ash g 8) b))
                           '(simple-array (unsigned-byte 32) 1))))
    (multiple-value-bind (frames resyncs)
        ;; capture 60 seconds of video (~48MB of rgba data?)
        (capture-frames l :count (* 9 60)
                        ;; convert to RGB with above LUT
                        :filter-generator 'sf-rgb-lut)
      ;; then write to an mp4 stream with pi hardware encoded using
      ;; pipe to ffmpeg
      (with-ffmpeg-stream (s "/tmp/foo.mp4")
        (write-sequence frames s))
      ;; return some indication of how well it kept up with recording,
      ;; non-zero means it lost sync with camera and had to wait for
      ;; resync (= # of tries it took to resync, at ~0.2sec each)
      resyncs)))

#++
(with-lepton (l)
  (reboot l))
