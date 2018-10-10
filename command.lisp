(in-package 3b-lepton)

(deftype octet () '(unsigned-byte 8))
(deftype word () '(unsigned-byte 16))
(deftype octet-vector () '(simple-array octet 1))
(deftype word-vector () '(simple-array word 1))

(declaim (inline word-vector))
(defparameter *no-wait* nil)
(defun word-vector (&rest r)
  (coerce r 'word-vector))
(defconstant +lepton-i2c+ #x2a)
(defconstant +protection-bit+ #x4000)
(defconstant +reg-power+ 0)
(defconstant +reg-status+ 2)
(defconstant +reg-command+ 4)
(defconstant +reg-data-length+ 6)
(defconstant +reg-data0+ 8)
(defconstant +reg-data1+ 10)
(defconstant +reg-data2+ 12)
(defconstant +reg-data3+ 14)
(defconstant +reg-data4+ 16)
(defconstant +reg-data5+ 18)
(defconstant +reg-data6+ 20)
(defconstant +reg-data7+ 22)
(defconstant +reg-data8+ 24)
(defconstant +reg-data9+ 26)
(defconstant +reg-data10+ 28)
(defconstant +reg-data11+ 30)
(defconstant +reg-data12+ 32)
(defconstant +reg-data13+ 34)
(defconstant +reg-data14+ 36)
(defconstant +reg-data15+ 38)
(defconstant +reg-crc-reg+ 40)
(defconstant +reg-data-block-1+ #xf800)
(defconstant +reg-data-block-2+ #xfc00)


(defun get-status* (lepton)
  (if (i2c lepton)
      (3b-i2c:read-register-16/be (i2c lepton)+lepton-i2c+ +reg-status+)
      :closed))

(defun get-status (lepton)
  (if (i2c lepton)
      (let* ((s (3b-i2c:read-register-16/be (i2c lepton)
                                            +lepton-i2c+ +reg-status+))
             (r (ldb (byte 8 8) s)))
        (list (gethash r *lep-errors* r)
              (if (logbitp 0 s) :busy :ready)
              (if (logbitp 1 s) :booted :booting)
              (if (logbitp 2 s) :boot-ok :booting?)
              s))
      '(:closed)))

(defun busy (lepton)
  (logbitp 0 (get-status* lepton)))

(defparameter *commands* '(:get 0 :set 1 :run 2))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mods*
    '(:agc (#x100
            :enable (#x0 2 2)
            :policy (#x4 2 2)
            :roi (#x08 4 4)
            :histogram (#xc 4)
            :heq-dampening-factor (#x24 1 1)
            :heq-clip-limit-high (#x2c 1 1)
            :heq-clip-limit-low (#x30 1 1)
            ;; heq-empty-counts gets undefined function error?
            :heq-empty-counts (#x3c 1 1)
            :heq-output-scale (#x44 2 2)
            :calculation-enable (#x48 2 2)
            :heq-linear-percent (#x4c 1 1)
            )
      :sys (#x200
            :ping (0 0)  ;; run
            :status (4 4) ;; get
            :serial-number (8 4)
            :uptime (#x0c 2)
            :aux-temp-kelvin (#x10 1)
            :fpa-temp-kelvin (#x14 1)
            :telemetry-enable (#x18 2 2)      ;; get/set
            :telemetry-location (#x1c 2 2)    ;; get/set
            #+:frame-average (#x20 0)         ;; run
            #+:num-frame-to-average (#x20 2 2) ;; get/set
            :customer-serial-number (#x28 16)  ;; get
            :video-scene-statistics (#x2c 4)   ;; get
            :roi-select (#x30 4 4)             ;; get/set x1,y1,x2,y2
            :thermal-shutdown-count (#x34 1)
            :shutter-position-control (#x39 2 2)
            :ffc-mode-control (#x3c 16 16)
            :run-ffc-normalization (#x40 0)
            :ffc-status (#x44 2)
            :gain-mode (#x48 2 2)
            :ffc-state (#x4c 2 2)
            :gain-mode-object (#x50 14 14))
      :vid (#x300
            :pseudo-color-lut-select (#x4 2 2)
            :user-lut (#x8 512 512)
            :focus-calculation-enable (#xc 2 2)
            :focus-roi-select (#x10 4 4)
            :focus-metric-threshold (#x14 2 2)
            :focus-metric (#x18 2 2)
            :video-freeze-enable (#x24 2 2)
            :video-output-format (#x30 2 2) ;;get/set
            :low-gain-pseudo-color-lut-select (#x34 2 2)) 

      :oem (#x800
            :power-down (0 0) ;; run
            :part-number (#x1c 16)
            :software-revision (#x20 4)
            :video-output-enable (#x24 2 2)
            :video-output-format (#x28 2 2)
            :video-source-select (#x2c 2 2)
            :customer-part-number (#x38 16)
            :video-output-source-constant (#x3c 1 1)
            :reboot (#x40 0) ;; run
            :ffc-normalization-target (#x44 1 1 0)
            :status (#x48 2)
            :frame-mean-intensity (#x4c 1)
            :gpio-mode-select (#x54 2 2)
            :gpio-vsync-phase-delay (#x58 2 2)
            :user-defaults (#x5c 2 nil 0)
            :restore-user-defaults (#x60 nil nil 0)
            :shutter-profile (#x64 2 2)
            :thermal-shutdown-enable (#x68 2 2)
            :bad-pixel-replacement-control (#x6c 2 2)
            :temporal-filter-control (#x70 2 2)
            :column-noise-filter-control (#x74 2 2)
            :pixel-noise-filter-control (#x78 2 2)
            )
      :rad (#xe00
            :rfbo-external-params (4 8 8) ;; get/set
            :radiometry-control-enable (#x10 2 2)
            :t-shutter-mode (#x24 2 2)
            :t-shutter-temp (#x28 1 1)
            :ffc-normalization (#x2c nil nil 0)
            :run-status (#x30 2)
            :flux-linear-parameters (#xbc 8 8)
            :t-linear-enable (#xc0 2 2)
            :t-linear-resolution (#xc4 2 2)
            :t-linear-auto-resolution (#xc8 2 2)
            :spotmeter-roi (#xcc 4 4)
            :spotmeter-value (#xd0 4)
            :low-gain-rfbo-external-params (#xd8 8 8)

            ))))

(defun i2c-command (lepton module base command data
                    &key protection (wait-count 1000)
                      (response-wait-count 1000))
 
  (let* ((i2c (i2c lepton))
         (cc (getf *commands* command))
         (mm (getf *mods* module))
         (bb (getf (cdr mm) base))
         (response-size (second bb))
         (words 0)
         (data-words))
    (assert (and cc mm bb response-size))
    #++(assert (not (busy i2c)))
    ;; wait for device to be ready
    (unless *no-wait*
      (loop for b = (get-status lepton)
         while (find :busy b)
         do (decf wait-count)
         when (minusp wait-count)
         do (error "device busy running command ~s ~s ~s? ~s~%"
                   command module base b)))
    (when data
      (etypecase data
        ((simple-array (unsigned-byte 32))
         (setf words (* 2 (length data)))
         (setf data-words (make-array words :element-type '(unsigned-byte 16)))
         (loop for i from 0 by 2
            for w across data
            ;; 32bit is stored as least sig word first 
            do (setf (aref data-words i) (ldb (byte 16 0) w))
              (setf (aref data-words (1+ i)) (ldb (byte 16 16) w))))
        ((simple-array (unsigned-byte 16))
         (setf words (length data))
         (setf data-words data))
        (octet-vector
         (error "octet data not implemented yet"))))
    (let ((command-word (logior (car bb) (car mm) cc
                                (if protection +protection-bit+ 0))))
      #++(format t "~&cmd ~4,'0x" command-word)
      (cond
        ;; fixme: move all these separate writes into a single rdwr call
        ((< 0 words 17)
         #++(format t "~&write ~s data words~%" words)
         (loop for r from +reg-data0+ by 2
            for d across data-words
            do (3b-i2c:write-register-16/be i2c +lepton-i2c+ r d)))
        ((< 16 words 512)
         (format t "~&write ~s data words~%" words)
         (loop for r from +reg-data-block-1+ by 2
            for d across data-words
            do (3b-i2c:write-register-16/be i2c +lepton-i2c+ r d))
         )
        ((plusp words)
         (error "too much data for command?")))
      #++(format t "~&write data length ~s~%" words)
      (3b-i2c:write-register-16/be i2c +lepton-i2c+ +reg-data-length+ words)
      #++(format t "~&write command ~4,'0x~%" cc)
      (3b-i2c:write-register-16/be i2c +lepton-i2c+ +reg-command+ command-word)
      ;; wait for results
      (if *no-wait*
          (list (get-status lepton) nil)
          (loop
             for ret = (get-status lepton)
             while (and (member :ok ret)
                        (member :busy ret))
             do (decf response-wait-count)
               (format t "~s ~s ~s: ~s~%" command module base ret)
               (sleep 0.1)
             when (minusp response-wait-count)
             do (error "no response from command? ~s ~s ~s = ~s"
                       command module base
                       ret)
             finally
               (return
                 (list ret
                       (loop repeat response-size
                          for reg from (if (<= response-size 16)
                                           +reg-data0+
                                           +reg-data-block-1+) by 2
                          collect (3b-i2c:read-register-16/be
                                   i2c +lepton-i2c+ reg)))))))))

(defmacro define-command (name module base command lambda-list &body body)
  (if (eq command :set)
      (alexandria:with-gensyms (lepton status r d r2)
        `(defun (setf ,name) (,(car lambda-list) ,lepton ,@ (cdr lambda-list))
           (let ((,d (progn ,@body)))
             (destructuring-bind ((,status &rest ,r) ,r2)
                 (i2c-command ,lepton ,module ,base ,command ,d
                              :protection ,(when (member module '(:oem :rad)) t))
               (if (eq ,status :ok)
                   ,r2
                   (error "command ~a failed : ~s (~s)" ',name ,status ,r))))))
      (alexandria:with-gensyms (lepton status r)
        `(defun ,name (,lepton)
           (destructuring-bind ((,status &rest ,r) ,lambda-list)
               (i2c-command ,lepton ,module ,base ,command nil
                            :protection ,(when (member module '(:oem :rad)) t))
             #++(format t "command ~s: ~s ~s~%" ',name ,status ,r)
             (if (eq ,status :ok)
                 (progn ,@body)
                 (error "command ~a failed : ~s (~s)" ',name ,status ,r)))))))

(declaim (inline make-signed-32))
(defun make-signed-32 (x)
  (if (logbitp 31 x)
      (dpb x (byte 32 0) -1)
      x))

(defmacro define-get-enum (name module base &body enums)
  (let ((get (alexandria:plist-hash-table
              (loop for e in enums for i from 0
                 collect (if (consp e) (second e) i)
                 collect (if (consp e) (first e) e)))))
    `(define-command ,name ,module ,base :get (lo hi)
       ;; enum is 32bit signed, so assemble it and fix sign
       (let ((v (dpb hi (byte 16 16) lo)))
         #++(format t "get ~s ~s -> ~s -> ~s~%"
                    lo hi v (gethash v ',get))
         (setf v (make-signed-32 v))
         (gethash v ',get (list :? v lo hi))))))

(defmacro define-get-set-enum (name module base &body enums)
  (let ((set (alexandria:plist-hash-table
              (loop for e in enums for i from 0
                 collect (if (consp e) (first e) e)
                 collect (if (consp e) (second e) i)))))
    `(progn
       (define-get-enum ,name ,module ,base ,@enums)
       (define-command ,name ,module ,base :set (v)
         #++(format t "~s -> ~s = ~s~%"
                    v
                    (alexandria:hash-table-plist ,set)
                    (gethash v ',set))
         (let ((x (gethash v ',set)))
           #++(format t "~s -> ~s = ~s~%"
                      ',set x
                      (word-vector (ldb (byte 16 0) x)
                                   (ldb (byte 16 16) x)))
           (word-vector (ldb (byte 16 0) x)
                        (ldb (byte 16 16) x)))))))


(defmacro define-get-set-int16 (name module base &key signed)
  `(progn
     (define-command ,name ,module ,base :get (x)
       ,@ (when signed
            `((when (logbitp 15 x)
                (setf x (dpb x (byte 16 0) -1)))))
       x)
     ,@ (when (third (getf (cdr (getf *mods* module)) base))
          `((define-command ,name ,module ,base :set (x)
              (word-vector x))))))

(defmacro define-get-set-int32 (name module base &key signed)
  `(progn
     (define-command ,name ,module ,base :get (lo hi)
       (let ((v (dpb hi (byte 16 16) lo)))
         ,@ (when signed
              `((setf v (make-signed-32 v))))
         v))
     (define-command ,name ,module ,base :set (x)
       (word-vector (ldb (byte 16 0) x)
                    (ldb (byte 16 16) x)))))

(defmacro define-get-set-roi (name module base)
  `(progn
     (define-command ,name ,module ,base :get (x1 y1 x2 y2)
       (list :x1 x1 :y1 y1 :x2 x2 :y2 y2))

     (define-command ,name ,module ,base :set (v)
       (if (and (consp v) (getf v :x1))
           (destructuring-bind (&key x1 y1 x2 y2) v
             (word-vector x1 y1 x2 y2))
           (coerce v 'word-vector)))))

(defmacro define-toggle (name module base)
  `(progn
     (define-get-enum ,name ,module ,base
       nil :enabled)
     ;; separate so we can use generalized boolean instead of exact enum
     (define-command ,name ,module ,base :set (v)
       (word-vector (if v 1 0) 0))))

;;; agc module
(define-toggle agc-enabled :agc :enable)

(define-get-set-enum agc-policy :agc :policy
  :linear :heq)

(define-get-set-roi agc-roi :agc :roi)

(define-command agc-histogram :agc :histogram :get (a b c d)
  (list :min a :max b :mean c :pixels d))

(define-get-set-int16 heq-dampening-factor :agc :heq-dampening-factor)

(define-get-set-int16 heq-clip-limit-high :agc :heq-clip-limit-high)

(define-get-set-int16 heq-clip-limit-low :agc :heq-clip-limit-low)

(define-get-set-int16 heq-empty-counts :agc :heq-empty-counts)

(define-get-set-enum heq-output-scale :agc :heq-output-scale
  :8-bits :14-bits)

(define-toggle agc-calculation-enabled :agc :calculation-enable)

(define-get-set-int16 heq-linear-percent :agc :heq-linear-percent)

;;; sys module

(define-command ping :sys :ping :run ()
  t)

(defparameter *status-state* '(0 :ready 1 :initializing 2 :low-power-mode
                               4 :entering-standby 5 :flat-field-in-progress))

(define-command status :sys :status :get (lo hi count reserved)
  (assert (zerop hi))
  (assert (zerop reserved))
  (list :status (getf *status-state* lo (list lo hi))
        :command-count count :reserved reserved))

(define-command serial-number :sys :serial-number :get (&rest r)
  (format nil "~{~2,'0x~^:~}" (reverse
                               (loop for i in r
                                  collect (ldb (byte 8 0) i)
                                  collect (ldb (byte 8 8) i)))))

(define-command uptime :sys :uptime :get (lo hi)
  (/ (dpb hi (byte 16 16) lo) 1000.0))

(define-command aux-temp-kelvin :sys :aux-temp-kelvin :get (k)
  (/ k 100.0))

(define-command fpa-temp-kelvin :sys :fpa-temp-kelvin :get (k)
  (/ k 100.0))

(declaim (inline k2c k2f))
(defun k2c (k) (- k 273.15))
(defun k2f (k) (+ (* (k2c k) 9/5) 32))

(defun aux-temp (lepton)
  (let ((k (aux-temp-kelvin lepton)))
    (list :k k :c (k2c k) :f (k2f k))))

(defun fpa-temp (lepton)
  (let ((k (fpa-temp-kelvin lepton)))
    (list :k k :c (k2c k) :f (k2f k))))

#++(define-command telemetry-enable :sys :telemetry-enable :get (lo hi)
  (assert (zerop hi))
  (ecase lo (0 nil) (1 :enabled)))

(define-toggle telemetry-enable :sys :telemetry-enable)

(define-get-set-enum telemetry-location :sys :telemetry-location
  :header :footer)

;; todo:
#++(define-command sys-frame-average :sys :frame-average :run ())
#++(define-command sys-num-frame-to-average
       :sys :num-frame-average :get (lo hi))
#++(define-command sys-num-frame-to-average
       :sys :num-frame-to-average :set (num))
(defmacro define-get-string (name module base)
  `(define-command ,name ,module ,base :get (&rest r)
     (format nil "~{~c~}" (loop for i in r
                             until (zerop i)
                             collect (code-char (ldb (byte 8 0) i))
                             when (plusp (ldb (byte 8 8) i))
                             collect (code-char (ldb (byte 8 8) i))))))

;; not not implemented (or not initialized?) on hardware?
(define-command customer-serial-number
    :sys :customer-serial-number :get (&rest r)
  (format nil "~{~2,'0x~^:~}" (reverse
                               (loop for i in r
                                  collect (ldb (byte 8 0) i)
                                  collect (ldb (byte 8 8) i)))))
#++
(define-get-string customer-serial-number :sys :customer-serial-number)

(define-command video-scene-statistics
    :sys :video-scene-statistics :get (min max mean count)
  (list :min min :max max :mean mean :pixel-count count))

(define-get-set-roi sys-roi-select :sys :roi-select)


(define-command thermal-shutdown-count :sys :thermal-shutdown-count :get (x)
  x)

(define-get-set-enum shutter-position-control :sys :shutter-position-control
  :idle :open :closed :brake-on (:unknown -1))

;; todo:
#++
(define-command ffc-mode-control :sys :ffc-mode-control :get (...))
#++
(define-command ffc-mode-control :sys :ffc-mode-control :set (...))

(define-command run-ffc-normalization :sys :run-ffc-normalization :run ())

(define-get-enum ffc-status :sys :ffc-status
  :ready :busy :frame-average-collecting-frames (:write-error -2) (:error -1))

(define-get-set-enum gain-mode :sys :gain-mode
  :high :low :auto)

(define-get-set-enum ffc-state :sys :ffc-state
  :never-commanded :imminent :in-progress :done)

;; todo
(define-command gain-mode-object :sys :gain-mode-object :get
    (x1 y1 x2 y2
        p-high-to-low p-low-to-high
        c-high-to-low c-low-to-high
        t-high-to-low t-low-to-high
        roi-population
        temp-enabled
        flux-threshold-low-to-high flux-threshold-high-to-low)
  (list :x1 x1 :y1 y1 :x2 x2 :y2 y2
        :p-high-to-low p-high-to-low :p-low-to-high p-low-to-high
        :c-high-to-low c-high-to-low :c-low-to-high c-low-to-high
        :t-high-to-low t-high-to-low :t-low-to-high t-low-to-high
        :roi-population roi-population
        :temp-enabled temp-enabled
        :flux-threshold-low-to-high flux-threshold-low-to-high
        :flux-threshold-high-to-low flux-threshold-high-to-low))
#++
(define-command  gain-mode-object :sys :gain-mode-object :set (...))

;;; vid

(define-get-set-enum color-lut-select :vid :pseudo-color-lut-select
  ;; fixme: figure out correct enum definition?
  :greyscale ;; docs imply this is 0
  :wheel6    ;; but say this is 0 in enum def?
  :fusion :rainbow :globow :sepia :color :ice-fire :rain :user)

;; this seems to lock up device?
#++
(define-command user-lut :vid :user-lut :get (&rest r)
  r)

(define-get-set-enum focus-calculation-enable :vid :focus-calculation-enable
  ;; fixme: make this boolean?
  :disable :enable)

(define-get-set-roi focus-roi :vid :focus-roi-select)

(define-get-set-int32 focus-metric-threshold
    :vid :focus-metric-threshold :signed nil)

(define-get-set-int32 focus-metric :vid :focus-metric :signed nil)

(define-toggle freeze :vid :video-freeze-enable)

(define-get-set-enum vid-output-format :vid :video-output-format
  ;; only :rgb888 and :raw14 supported by hardware currently?
  :raw8 :raw10 :raw12 :rgb888 :rgb666 :rgb565 :yuv422_8bit
  :raw14 :yuv422_10bit :user-defined
  :raw8-2 :raw8-3 :raw8-4 :raw8-5 :raw8-6)

(define-get-set-enum low-gain-color-lut-select
    :vid :low-gain-pseudo-color-lut-select
  ;; fixme: figure out correct enum definition?
  :greyscale ;; docs imply this is 0
  :wheel6    ;; but say this is 0 in enum def?
  :fusion :rainbow :globow :sepia :color :ice-fire :rain :user)


;;; oem
;; todo: (shutdown and reboot shouldn't wait for return status
(defun shutdown (l)
  (declare (ignore l))
  (error "not implemented yet"))

#++
(define-command reboot :oem :reboot :run ())
(defun reboot (l)
  (declare (ignore l))
  (error "not implemented yet"))

(define-get-string oem-part-number :oem :part-number)

(defvar *part-numbers*
  (alexandria:plist-hash-table '("500-0643-00" "Lepton 1.5"
                                 "500-0690-00" "Lepton 1.6"
                                 "500-0659-01" "Lepton 2.0"
                                 "500-0763-01" "Lepton 2.5"
                                 "500-0726-01" "Lepton 3.0"
                                 "500-0771-01" "Lepton 3.5")
                               :test 'equal))

(defun lookup-part-number (l)
  (gethash (oem-part-number l) *part-numbers*))

(define-command oem-software-revisions :oem :software-revision :get (a b c d)
  (list :gpp (list (ldb (byte 8 0) a) (ldb (byte 8 8) a) (ldb (byte 8 0) b))
        :dsp (list (ldb (byte 8 8) b) (ldb (byte 8 0) c) (ldb (byte 8 8) c))
        :reserved d))

(define-toggle video-output-enable :oem :video-output-enable)

(define-get-set-enum oem-video-output-format :oem :video-output-format
  ;; not sure how this differs from  :vid :video-output-format
  ;; only :rgb888 and :raw14 supported by hardware currently?
  :raw8 :raw10 :raw12 :rgb888 :rgb666 :rgb565 :yuv422_8bit
  :raw14 :yuv422_10bit :user-defined
  :raw8-2 :raw8-3 :raw8-4 :raw8-5 :raw8-6)

(define-get-set-enum oem-video-source :oem :video-source-select
  :raw :cooked :ramp :constant :ramp-h :ramp-v :ramp-custom
  ;; frame average, freeze-frame
  :frame-capture :frame-freeze
  ;; reserved
  :frame-0 :frame-1 :frame-2 :frame-3 :frame-4)

;; may not be set?
(define-get-string oem-customer-part-number :oem :customer-part-number)

(define-get-set-int16 oem-video-output-source-constant
    :oem :video-output-source-constant)

(define-get-set-int16 oem-ffc-normalization-target
    :oem :ffc-normalization-target)
;; todo:
#++
(define-command oem-ffc-normalize :oem :ffc-normalization-target :run ()
  ...)

(define-get-enum oem-status :oem :status
  :ready :busy :frame-average-collecting-frames (:otp-write0error -2)
  (:error -1))

(define-get-set-int16 frame-mean-intensity :oem :frame-mean-intensity)

(define-get-enum gpio-mode-select :oem :gpio-mode-select
  :gpio :i2c-master :spi-master-vlb-data :spio-master-reg-data
  :spi-slave-vlb-data :vsync)

(define-get-set-int32 gpio-vsync-phase-delay
    :oem :gpio-vsync-phase-delay :signed t)

(define-get-enum user-defaults :oem :user-defaults
  :not-written :written)
;; todo
#++
(define-command save-user-defaults :oem :user-defaults :run ()
  ;; need to set VPROG voltage for this, not sure how/if we can do that
  ?)
#++
(define-command restore-user-defaults :oem :restore-user-defaults :run ())

(define-command shutter-profile :oem :shutter-profile :get (close open)
  (list :close close :open open))

(define-command shutter-profile :oem :shutter-profile :set (l)
  (word-vector (getf l :close) (getf l :open)))

(define-toggle thermal-shutdown-enable :oem :thermal-shutdown-enable)

(define-toggle bad-pixel-replacement-control
    :oem :bad-pixel-replacement-control)

(define-toggle temporal-filter-control :oem :temporal-filter-control)

(define-toggle column-noise-filter-control :oem :column-noise-filter-control)

(define-toggle pixel-noise-filter-control :oem :pixel-noise-filter-control)



;;; RAD module
(define-command rfbo-external-params :rad :rfbo-external-params :get (&rest r)
  (list :R (logior (pop r) (ash (pop r) 16))
        :B (/ (logior (pop r) (ash (pop r) 16)) 1000.0)
        :F (/ (logior (pop r) (ash (pop r) 16)) 1000.0)
        :O (/ (make-signed-32 (logior (pop r) (ash (pop r) 16))) 1000.0)))
;; todo: set rfbo


(define-toggle radiometry-control :rad :radiometry-control-enable)

(define-get-set-enum t-shutter-mode :rad :t-shutter-mode
  :user :cal :fixed)

(define-get-set-int16 t-shutter-temperature :rad :t-shutter-temp)

;; todo
#++
(define-command rad-ffc-normalization :rad :ffc-normalization :run ()
  )

(define-get-enum rad-run-status :rad :run-status
  :ready :busy :frame-average-collecting-frames (:error -1))

(define-command flux-linear-parameters
    :rad :flux-linear-parameters :get (&rest r)
  (flet ((s8 (x) (/ x (float 8192/100)))
         (s1 (x) (/ x 100.0)))
    (list :scene-emissivity (s8 (pop r))
          :t-bg-k (s1 (pop r))
          :tau-window (s8 (pop r))
          :t-window-k (s1 (pop r))
          :tau-atm (s8 (pop r))
          :t-atm-k (s1 (pop r))
          :refl-window (s8 (pop r))
          :t-refl-k (s1 (pop r)))))

;; todo: set flux-linear-parameters

(define-toggle t-linear :rad :t-linear-enable)

(define-get-set-enum t-linear-resolution :rad :t-linear-resolution
  :0.1 :0.01)

(define-toggle t-linear-auto-resolution :rad :t-linear-auto-resolution)

(define-get-set-roi spotmeter-roi :rad :spotmeter-roi)

(define-command spotmeter-value* :rad :spotmeter-value :get (a b c d)
  ;; todo: scale these by tlinear resolution
  (list :value a :max b :min c :population d))

(defun spotmeter-value (l &key (units :k))
  (let ((s (ecase (t-linear-resolution l)
             (:|0.1| 0.1)
             (:|0.01| 0.01)))
        (v (spotmeter-value* l)))
    (flet ((s (x)
             (ecase units
               (:k (* x s))
               (:c (k2c (* x s)))
               (:f (k2f (* x s))))))
      (destructuring-bind (&key value min max population) v
        (if (and value min max population)
            (list :value (s value) :max (s max) :min (s min)
                  :population population)
            v)))))

(define-command low-gain-rfbo-external-params :rad :low-gain-rfbo-external-params :get (&rest r)
  (list :R (logior (pop r) (ash (pop r) 16))
        :B (/ (logior (pop r) (ash (pop r) 16)) 1000.0)
        :F (/ (logior (pop r) (ash (pop r) 16)) 1000.0)
        :O (/ (make-signed-32 (logior (pop r) (ash (pop r) 16))) 1000.0)))
;; todo: set low-gain rfbo
