#++
(ql:quickload '(3b-lepton))
(in-package 3b-lepton)

(defclass lepton ()
  ((spi :initform nil :initarg :spi :reader spi)
   (i2c :initform nil :initarg :i2c :reader i2c)
   ;; fixme :read this from device
   (rows :initform 120 :reader rows)
   (cols :initform 160 :reader cols)))

(defmethod initialize-instance :after ((l lepton) &key)
;;; wait a bit for device to boot/error if not boot OK?
;;; read rows,cols
  
  )

(defun open-lepton (&key (spi "0.0") (i2c "1"))
  (let (s i)
    (unwind-protect
	 (prog1
	     (make-instance
	      'lepton
	      :spi (setf s (cl-spidev:open spi))
	      :i2c (setf i (3b-i2c:open-i2c i2c)))
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

(defmacro with-lepton ((l &key (spi "0.0") (i2c "1")) &body body)
  `(let ((,l (open-lepton :spi ,spi :i2c ,i2c)))
     (unwind-protect
	  (progn
	    ,@body)
       (close-lepton ,l))))
