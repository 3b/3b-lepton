(in-package 3b-lepton)
#++(ql:quickload 'bordeaux-threads)
#++(require 'sb-concurrency)
;;; rewriting (some of) capture.lisp for reading in a separate thread

;;; for now assuming sbcl, so using sb-concurrency queue to send array
;;; back to reader thread (though bx-threads elsewhere to minimized
;;; sbcl specific code)

;; processing is divided into 2 parts:
;;; reader thread synchronizes and reads data segments from
;;; hardware. when a segment is complete (or possibly on sync failure), a
;;; message is sent to status queue.
;;
;;; processing function is called from user thread, and (possibly
;;; optionally) blocks on status queue. when a segment complete
;;; message is read, the segment is decoded into proper part of frame
;;; data array (with optional color remapping?). processing function
;;; returns values indicating completion of frame and modified region.
;;; user code can either process each segment as it is received (for
;;; example with interactive display), or wait for completed frames
;;; (for example when encoding video) as needed.


;; fixme: use a mutex on this
(defparameter *message* :not-started)
(defparameter *segment-count* 0)
(defun start-reader-thread (thread-data)
  (bordeaux-threads:make-thread
   (lambda ()
     (setf *segment-count* 0)
     (destructuring-bind (lepton segment-buffer semaphore) (cdr thread-data)
       (declare (type (simple-array (unsigned-byte 8) (*)) segment-buffer))
       (unwind-protect
            (let* ((fd (cl-spidev::handle-stream (spi lepton)))
                   (p (calculate-packet-params lepton))
                   (bytes-per-packet (getf p :length))
                   (packets-per-frame (getf p :packets))
                   (segments-per-frame (getf p :segments))
                   (packets-per-segment (/ packets-per-frame segments-per-frame))
                   (bytes-per-segment (* bytes-per-packet packets-per-segment))
                   ;; fixme: detect or make configurable
                   (max-ioctl-bytes 4096)
                   (packets-per-read (floor max-ioctl-bytes bytes-per-packet))
                   (max-segments (floor (length segment-buffer)
                                        bytes-per-segment))
                   ;; offsets (in segments/packets) into segment buffer of
                   ;; location into which we are currently reading
                   (segment-index 0)
                   (packet-index 0)
                   ;; max 'discard' packets before we give up
                   (max-discard 2000)
                   ;; # of discard segments seen in a row
                   (discard-segments 0)
                   ;; # of discard segments in a row before we give up
                   ;; (we get ~23 frames of discard during FFC every 3
                   ;; min by default, so allow that many. probably
                   ;; should try to detect FFC and signal processing
                   ;; thread, or maybe send a 'repeat' message every 4
                   ;; just to maintain consistent message rate)
                   (max-discard-segments (* 23 4)))
              (declare (type (unsigned-byte 8)
                             segments-per-frame
                             packets-per-segment)
                       (type (unsigned-byte 16)
                             packets-per-frame
                             bytes-per-packet bytes-per-segment
                             segment-index max-segments
                             packet-index)
                       (fixnum discard-segments))
              (format t "max-segments = ~s~%" max-segments)
              (cl-spidev-lli::with-transfer-buffers (ctb
                                                     segment-buffer
                                                     bytes-per-packet
                                                     (1+
                                                      (* packets-per-segment
                                                         max-segments))
                                                     ;; use lower
                                                     ;; speed to
                                                     ;; reduce CPU
                                                     *default-speed*
                                                     #++ 10000000
                                                     0 8)
                (locally (declare (optimize speed))
                  (flet ((skip-discard ()
                           (loop with index = (* packet-index bytes-per-packet)
                                 repeat max-discard
                                 do (read-packet fd ctb packet-index 1)
                                 while (is-discard segment-buffer index))
                           (locally (declare (optimize (Speed 1)))
                             (assert (= (* packet-index bytes-per-packet)
                                        (ctb-offset ctb packet-index))))
                           (when (is-discard segment-buffer
                                             (* packet-index bytes-per-packet))
                             (error "broken?")))
                         (read-segment ()
                           ;; skip-discard read 1 packet already
                           (incf packet-index)
                           (assert (= packet-index
                                      (1+ (* segment-index packets-per-segment))))
                           ;; read remaining packets of segment
                           (loop with end-index = (* (1+ segment-index)
                                                     packets-per-segment)
                                 for c = (min (- end-index packet-index)
                                              packets-per-read)
                                        ;for i fixnum below 100000
                                 do (assert (plusp c))
                                    (read-packet fd ctb packet-index c)
                                    (incf packet-index c)
                                 while (< packet-index end-index)
                                 finally (assert (= packet-index end-index)))
                           ;; check for discard segment
                           (let ((o (* bytes-per-packet
                                       (+ (* segment-index packets-per-segment)
                                          20))))
                             ;; if packet # is wrong, we are out of sync
                             (unless (= 20 (aref segment-buffer (1+ o)))
                               ;; complain and resync
                               (setf *message* :sync3)
                               (bt:signal-semaphore semaphore)
                               (setf discard-segments 0)
                               (sleep 0.185)
                               (return-from read-segment))
                             ;; if segment # is 0, it is discard segment
                             (when (zerop (ldb (byte 4 4)
                                               (aref segment-buffer o)))
                               ;; see if we should complain
                               (incf discard-segments)
                               (when (>= discard-segments max-discard-segments)
                                 (setf *message* :sync2)
                                 (bt:signal-semaphore semaphore)
                                 (fill segment-buffer #x3b)
                                 (setf discard-segments 0)
                                 (sleep 0.185))
                               (return-from read-segment)))
                           #++
                           (locally (declare (optimize debug (speed 1)))
                            (let* ((packet-offset (* segment-index
                                                    packets-per-segment))
                                  (s (ldb (byte 4 4)
                                          (aref segment-buffer
                                                (* (+ packet-offset 20)
                                                   bytes-per-packet)))))
                              (assert (< 0 s 5))))
                           ;; tell processing thread there is a segment
                           ;; (and where it is)
                           (setf *message*
                                 (* segment-index packets-per-segment))
                           (bt:signal-semaphore semaphore)
                           #++(sb-concurrency:send-message
                            ;; send offset of data in packets from
                            ;; start of segment-buffer
                            mailbox (* segment-index packets-per-segment))
                           ;; move to next segment
                           (setf discard-segments 0)
                           (incf *segment-count*)
                           (setf segment-index
                                 (mod (1+ segment-index) max-segments))))
                    (loop
                      do (setf packet-index
                               (* segment-index packets-per-segment))
                         (assert (< (* (1+ packet-index) bytes-per-packet)
                                    (length segment-buffer)))
                         (assert (not (eql *message* packet-index)))
                         (skip-discard)
                      if (lost-sync segment-buffer
                                    (* packet-index bytes-per-packet)
                                    bytes-per-packet)
                        ;; tell processing thread, then resync
                        do #++(sb-concurrency:send-message mailbox :sync)
                           (setf *message* :sync)
                           (bt:signal-semaphore semaphore)
                           (setf discard-segments 0)
                           (sleep 0.185)
                      else
                        ;; read the segment and tell processing thread
                        do (read-segment)
                      until (car thread-data))))))
         ;; send a message to processing thread so it knows if reader
         ;; thread exits early
         (setf *message* :quit)
         (bt:signal-semaphore semaphore))))
   :name "lepton-reader-thread"
   :initial-bindings (list* (cons '*standard-output* *standard-output*)
                            bordeaux-threads:*default-special-bindings*)))

(defun call-with-reader-thread (thunk thread-data)
  (let ((thread (start-reader-thread thread-data)))
    (unwind-protect (funcall thunk)
      (setf (car thread-data) t)
      (ignore-errors (bordeaux-threads:join-thread thread)))))

(defparameter *foo* 4)
(defun copy-segment (dest segment-buffer packet-offset
                     pixel-filter bytes-per-packet)
  #++
  (when (plusp *foo*)
    #++(format t "~&copy segment at ~s ~2,'0x~%"
               offset (subseq segment-buffer offset (+ 16 offset)))
    (format t "~&copy segment at ~s:~%" packet-offset)
    (loop for i below 60
          for o = (* (+ packet-offset i) bytes-per-packet)
          do (format t " ~s: @~s ~2,'0x~%"
                     i o (subseq segment-buffer o (+ 20 o))))
    (decf *foo*))
  #++
  (let ((o (* (+ packet-offset 20) bytes-per-packet)))
    (format t "~&copy segment at ~s: ~2,'0x~%"
            packet-offset (subseq segment-buffer o (+ 10 o))))
  #++(values 0 0 0 nil)

  (let ((segment (ldb (byte 4 4)
                      (aref segment-buffer
                            (* (+ packet-offset 20)
                               bytes-per-packet)))))
    (assert (< 0 segment 5))
    (multiple-value-bind (y1 y2 telemetry)
        (funcall pixel-filter segment-buffer dest (1- segment) packet-offset)
      ;; segment index, start index, end index, decoded telemetry
      ;(format t " -> ~s~%" *message*)
      (values segment y1 y2 telemetry))))

(defun decode-telemetry (buffer offset)
  (declare (ignore buffer offset))
  '(:todo :telemetry))

(defmacro define-segment-filter (name (var &key (element-type
                                                 '(unsigned-byte 16))
                                             (samples 1))
                                 &body body)
  `(defun ,name (telemetry
                 pixels-per-packet packets-per-segment
                 pixels-per-row bytes-per-packet)
     (declare (type (unsigned-byte 16)
                    packets-per-segment pixels-per-row
                    pixels-per-packet bytes-per-packet))
     (macrolet
         ((body (tx)
            `(lambda (input-buffer output-buffer segment input-offset
                      &key (output-offset 0))
               (declare (type (simple-array ,',element-type 1)
                              output-buffer)
                        (type (simple-array (unsigned-byte 8) 1) input-buffer)
                        (type (integer 0 5) segment)
                        (type fixnum input-offset output-offset)
                        (optimize speed))
               ;; adjust packet/pixel ranges for telemtry
               ;; header/footer, if any
               (let* ((start-packet ,(if (eq tx :header)
                                         `(if (zerop segment)
                                              4
                                              0)
                                         0))
                      (end-packet  ,(if (eq tx :footer)
                                        `(if (= segment 3)
                                             (- packets-per-segment 4)
                                             packets-per-segment)
                                        'packets-per-segment))
                      (pixel-offset0 (* segment packets-per-segment))
                      (pixel-offset1 (* pixels-per-packet
                                        (max 0
                                             ,(if (eql tx :header)
                                                  '(if (zerop segment)
                                                    pixel-offset0
                                                    (- pixel-offset0 3))
                                                  'pixel-offset0))))
                      ;; range of modified rows, in case caller wants
                      ;; to display segments as they arrive instead of
                      ;; waiting for full frame
                      (y1 (floor pixel-offset1 pixels-per-row))
                      (y2 (ceiling (+ pixel-offset1
                                      (* pixels-per-packet
                                         (- end-packet start-packet)))
                                   pixels-per-row))
                      (pixel-offset (+ output-offset pixel-offset1)))
                 (cffi:with-pointer-to-vector-data (in-pointer input-buffer)
                   (loop
                     with len of-type segment-size-type
                       = bytes-per-packet
                     for packet of-type (unsigned-byte 8)
                     from start-packet below end-packet
                     for base of-type buffer-base
                       = (* bytes-per-packet
                            (+ input-offset packet))
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
                        (incf pixel-offset pixels-per-packet)))
                 (values
                  y1 y2
                  ,(case tx
                     (:header
                      '(when (zerop segment)
                        (decode-telemetry input-buffer input-offset)))
                     (:footer
                      '(when (= 3 segment)
                        (decode-telemetry input-offset (+ input-offset ))))
                     (t nil)))))))
       (values
        ;; return a function to filter a segment
        (cond
          ((eq telemetry :header) (body :header))
          ((eq telemetry :footer) (body :footer))
          (t (body nil)))
        ;; and info to allocate correct type of buffer for output
        ',element-type
        ',samples))))


(define-segment-filter sf-rgb-lut (v :element-type (unsigned-byte 8)
                                     :samples 4)
  (let ((c (aref *rgb-lut* v)))
    (values (ldb (byte 8 24) c)
            (ldb (byte 8 16) c)
            (ldb (byte 8 8) c)
            (ldb (byte 8 0) c))))

(define-segment-filter sf-raw (v :element-type (unsigned-byte 16)
                                 :samples 1)
  v)

(defun make-segment-buffer (lepton buffer-segments)
  (let* ((p (calculate-packet-params lepton))
         (bytes (* (/ (getf p :packets)
                      (getf p :segments))
                   (getf p :length)
                   buffer-segments)))
    (make-array bytes
                :element-type '(unsigned-byte 8)
                :initial-element #x3b)))

(defun check-configuration (lepton)
  ;;; assert some assumptions on lepton config before starting
  (update-metadata lepton)
  (assert (eq (video-format lepton) :raw14))

  )
(defmacro with-reader-thread ((lepton frame-var
                               &key (filter-generator 'sf-rgb-lut))
                              &body body)
  (alexandria:once-only (lepton)
    (alexandria:with-gensyms (thread-data
                              queue segment
                              segment-buffer telemetry
                              filter output-type output-samples
                              params bytes-per-packet)
      `(progn
         ;(setf *foo* 4)
         (check-configuration ,lepton)
         (let ((,params (calculate-packet-params ,lepton)))
          (multiple-value-bind (,filter ,output-type ,output-samples )
              (,filter-generator (telemetry ,lepton)
                                 (getf ,params :pixels)
                                 (/ (getf ,params :packets)
                                    (getf ,params :segments))
                                 (cols ,lepton)
                                 (getf ,params :length))
            (let* ((,frame-var (make-array (* (rows ,lepton)
                                              (cols ,lepton)
                                              ,output-samples)
                                           :element-type ,output-type
                                           :initial-element #x3b))
                   (,queue (bt:make-semaphore
                            :name "lepton-reader-status"))
                   (,segment 0)
                   ;; store enough for 4 segments (should only need 2 if
                   ;; read-frame is called often enough, but allow for some
                   ;; extra slack)
                   (,segment-buffer (make-segment-buffer ,lepton 4))
                   (,telemetry nil)
                   (,thread-data (list nil ,lepton ,segment-buffer ,queue))
                   (,bytes-per-packet (getf ,params :length)))
              (flet ((read-frame ()
                       (let ((s (bt:wait-on-semaphore ,queue))
                             (m *message*))
                         (declare (ignorable s))
                         #++(format t "message ~s, length ~s~%"
                                 m s)
                         ;;(format t "~s ~s" m *segment-count*)
                         (if (symbolp m)
                             (values nil nil nil nil m)
                             (multiple-value-bind (s i1 i2 telemetry)
                                 (copy-segment ,frame-var ,segment-buffer m
                                               ,filter ,bytes-per-packet)
                               (when telemetry
                                 (setf ,telemetry telemetry))
                               (if (= s (1+ ,segment))
                                   (incf ,segment)
                                   (setf ,segment 0))
                               (if (>= ,segment 4)
                                   (progn
                                     (setf ,segment 0)
                                     (values t i1 i2 ,telemetry nil))
                                   (values nil i1 i2 ,telemetry nil))))
                         ;;(format t " -> ~s ~s~%" *message* *segment-count*)
                         ))
                     (exit-reader ()
                       (setf (car ,thread-data) t)))
                (declare (ignorable #'exit-reader))
                (call-with-reader-thread
                 (lambda () ,@body)
                 ,thread-data)))))))))

#++
(with-lepton  (l :i2c "0")
  (with-reader-thread (l f)
    (time
     (loop for i below 1200
           for (framep start end telemetry error) = (multiple-value-list
                                                     (read-frame))
           when framep
             do #++(save-frame f)
                (format t "#~s~%" telemetry)
           when error
             do (format t "@~s~s~%" i error)
           finally (format t "exiting...~%") (exit-reader)))))


#++
(with-lepton (l :i2c "0")
  (list (calculate-packet-params l)
        (rows l)
        (cols l)))

#++
((:LENGTH 164 :PACKETS 240 :SEGMENTS 4 :PIXELS 80) 120 160)
