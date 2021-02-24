#++
(ql:quickload '(3b-lepton))
(in-package 3b-lepton)

(defclass lepton ()
  ((spi :initform nil :initarg :spi :reader spi)
   (i2c :initform nil :initarg :i2c :reader i2c)
   ;; fixme :read this from device
   (rows :initform 120 :reader rows)
   (cols :initform 160 :reader cols)
   (video-format :initform :raw14 :reader video-format)
   (telemetry :initform nil :reader telemetry)))

(defvar *part-metadata*
  (alexandria:plist-hash-table '("500-0643-00" (80 60)
                                 "500-0690-00" (80 60)
                                 "500-0659-01" (80 60)
                                 "500-0763-01" (80 60)
                                 "500-0726-01" (160 120)
                                 "500-0771-01" (160 120))
                               :test 'equal))

(defun update-metadata (l &key rows cols format)
  (let ((s (get-status l)))
    (cond
      ((and rows cols format)
       (setf (slot-value l 'cols) cols)
       (setf (slot-value l 'rows) rows)
       (setf (slot-value l 'video-format) format))
      ((and (find :boot-ok s) (find :booted s) (not (find :busy s)))
       (let* ((part (oem-part-number l))
              (m (gethash part *part-metadata*)))
         (unless m
           (error "unrecognized part # ~s? can't set metadata" part))
         (setf (slot-value l 'cols) (first m))
         (setf (slot-value l 'rows) (second m))
         (setf (slot-value l 'video-format)
               (vid-output-format l))
         (setf (slot-value l 'telemetry)
               (telemetry-enable l))))
      ((and (find :boot-ok s) (find :booted s))
       (error "device busy ~s~%" s))
      (t
       (error "bad status ~s?" s)))))

(defmethod initialize-instance :after ((l lepton) &key)
;;; wait a bit for device to boot/error if not boot OK?
;;; read rows,cols
  (update-metadata l)
  )

(defvar *file-debug* nil)
(defun open-lepton (&key (spi "0.0") (i2c "0"))
  (let (s i)
    (unwind-protect
         (prog1
             (make-instance
              'lepton
              :spi (setf s (cl-spidev:open spi))
              :i2c (setf i (3b-i2c:open-i2c i2c)))
           (push (list s i) *file-debug*)
           (setf s nil i nil))
      ;; close devices if we got an error before returning
      (when s (cl-spidev:close s))
      (when i (3b-i2c:close-i2c i)))))

(defun close-lepton (l)
  (when (spi l)
    (cl-spidev:close (spi l))
    (setf (slot-value l 'spi) nil))
  (when (i2c l)
    (3b-i2c:close-i2c (i2c l))
    (setf (slot-value l 'i2c) nil)))

(defmacro with-lepton ((l &key (spi "0.0") (i2c "0")) &body body)
  `(let ((,l (open-lepton :spi ,spi :i2c ,i2c)))
     (unwind-protect
          (progn
            ,@body)
       (close-lepton ,l))))
