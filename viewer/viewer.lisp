#++ (ql:quickload '3b-lepton/viewer)
(in-package 3b-lepton/viewer)
(let ((swap nil))
  (defun glx-swap-interval-mesa (i)
    (unless swap
      (setf swap
            (glop-glx::get-proc-address "glXSwapIntervalMESA")))
    (when swap
      (cffi:foreign-funcall-pointer swap () :unsigned-int i))))

(defclass viewer (glop:window)
  ((lepton :initform nil :accessor lepton)
   (textures :initform nil :accessor textures)
   (shaders :initform nil :accessor shaders)))

(defmethod glop:on-event ((w viewer) (event glop:key-event))
  (format t "Key ~:[released~;pressed~]: ~A~%"
          (glop:pressed event) (glop:keysym event))
  (when (eq (glop:keysym event) :escape)
    (glop:push-close-event w)))

(defun update-lut ()
  (loop for i from 27000 to 32000
     for x from 0
     for r = (floor (* 255 0.25 (+ 2 (sin (* x (/ pi 4096)))
                                   (cos (* x (/ pi 512))))))
     for g = (floor (* 255 0.5 (1+ (sin (* (+ x 1234) (/ pi 2048))))))
     for b = (floor (* 255 0.5 (1+ (cos (* x (/ pi 1024))))))
     do (setf (aref 3b-lepton::*rgb-lut* i)
              (logior (ash b 24) (ash g 16) (ash r 8) 255)))
  #++
  (loop for i from 30000 to 30500 ;; ~skin temp range
     for y from 0
     for x = (ash y 0)
     for r = (* 255 (ldb (byte 1 7) x))
     for g = (* 63 (ldb (byte 2 5) x))
     for b = (* 32 (ldb (byte 3 7) x))
     for a = 255
     do (setf (aref 3b-lepton::*rgb-lut* i)
              (logior (ash r 24) (ash g 16) (ash b 8) a))))

(defparameter *fs* t)
(defmethod glop:on-event ((w viewer) (event glop:button-event))
  (format t "Button ~:[released~;pressed~]: ~S~%"
          (glop:pressed event) (glop:button event))
  (when (glop:pressed event)
    (setf *fs* (not *fs*))
    (glop:set-fullscreen w *fs*))
  ;(glx-swap-interval-mesa 2)
  (update-lut))

(defmethod glop:on-event ((w viewer) (event glop:mouse-motion-event))
  (declare (ignore event))
  )

(defmethod glop:on-event ((w viewer) (event glop:resize-event))
  (gl:viewport 0 0 (glop:width event) (glop:height event))
  (format t "Resize: ~Sx~S~%" (glop:width event) (glop:height event)))

(defmethod glop:on-event ((w viewer) (event glop:expose-event))
  (declare (ignore event))
  )

(defmethod glop:on-event ((w viewer) (event glop:close-event))
  (declare (ignore event))
  )


(defun upload-frame (w buffer)
  #++(gl:bind-texture :texture-2d (car (textures w )))
  #++(gl:tex-image-2d :texture-2d 0
                      :luminance16 160 120 0 :luminance :unsigned-short
                      buffer)
  (cffi:with-pointer-to-vector-data (p buffer)
    (gl:tex-image-2d :texture-2d 0
                     :rgba 160 120 0 :rgba :unsigned-byte
                     p)))

(defun upload-frame-part (w buffer y1 y2)
  #++(gl:bind-texture :texture-2d (car (textures w )))
  (cffi:with-pointer-to-vector-data (p buffer)
    (gl:tex-sub-image-2d :texture-2d 0
                         0 y1 160 (- y2 y1) :rgba :unsigned-byte
                         (cffi:inc-pointer p (* y1 160 4)))))

(defun draw (w)
  ;(gl:clear-color (random 1.0) (random 1.0) (random 1.0) 1)
  ;(gl:clear :color-buffer)
  ;(gl:color 1 1 1)
  #++(gl:enable :texture-2d :blend)
  
  (gl:with-pushed-matrix* (:modelview)
    ;(gl:scale 0.5 0.5 0.5)
   (gl:with-primitive :polygon
     (gl:tex-coord 0.0 1.0)
     (gl:vertex 0.0 0.0 0)
     (gl:tex-coord 1.0 1.0)
     (gl:vertex 1 0.0 0)
     (gl:tex-coord 1.0 0.0)
     (gl:vertex 1 1 0)
     (gl:tex-coord 0.0 0.0)
     (gl:vertex 0.0 1 0))))

(* 800 480 60)
23 040 000

(defun viewer ()
  (3b-lepton::with-lepton (lepton :i2c "0")
    (glop:with-window (w "3b-lepton" 640 480  :win-class 'viewer :x 0 :y 0)
      (setf (lepton w) lepton)
      #++(format t "~&~s~%"
                 (gl:get-string :extensions ))
      ;;(glop:swap-interval w 0)
      ;;(glx-swap-interval-mesa 0)
      (gl:clear-color 0.3 0.3 0.3 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      (setf (textures w)
            (gl:gen-textures 2))
      (gl:bind-texture :texture-2d (first (textures w )))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:enable :texture-2d)
      (let ((3b-lepton::*rgb-lut* (copy-seq 3b-lepton::*rgb-lut*) ))
        (update-lut)
        (3b-lepton::with-capture (lepton
                                  frames-buffer
                                  :count 1
                                  :filter-generator '3b-lepton::pf-rgb-lut)
          (upload-frame w frames-buffer)
          (loop
            with last = (get-internal-real-time)
            with frames = 0
            while (glop:dispatch-events w :blocking nil :on-foo nil)
            do
               (incf frames)
               (when (> (- (get-internal-real-time) last)
                        (* 4 internal-time-units-per-second))
                 (setf last (get-internal-real-time))
                 (format t "~s fps~%" (/ frames 4.0))
                 (force-output)
                 (setf frames 0))
               #++(3b-lepton::read-frame)
               (draw w)
            #++(upload-frame-part w frames-buffer 0 120)
            #++(glop:swap-buffers w)
               (glop-glx:glx-swap-buffers (glop::x11-window-display w)
                                          (glop::x11-window-id w))
               #++(swank::process-requests t)))))))


(defun threaded-viewer ()
  ;; get rid of some style-warnings (fixme: figure out what causes them)
  (declare (notinline gl:matrix-mode gl:bind-texture))
  (unless (sb-posix:getenv "DISPLAY")
    (sb-posix:setenv "DISPLAY" ":0.0" 1))
  (3b-lepton::with-lepton (lepton :i2c "0")
    (glop:with-window (w "3b-lepton" 640 480  :win-class 'viewer :x 0 :y 0)
      (setf (lepton w) lepton)
      #++(format t "~&~s~%"
                 (gl:get-string :extensions ))
      (glop:swap-interval w 0)
      (gl:clear-color 0.3 0.3 0.3 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)
      (setf (textures w)
            (gl:gen-textures 2))
      (gl:bind-texture :texture-2d (first (textures w)))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:bind-texture :texture-2d (second (textures w )))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:enable :texture-2d)
      (let ((3b-lepton::*rgb-lut* (copy-seq 3b-lepton::*rgb-lut*)))
        (update-lut)
        (3b-lepton::with-reader-thread (lepton frame)
          (gl:bind-texture :texture-2d (first (textures w )))
          (upload-frame w frame)
          (gl:bind-texture :texture-2d (second (textures w )))
          (upload-frame w frame)
          (loop
            with last = (get-internal-real-time)
            with frames = 0
            with gc-epoch = SB-KERNEL::*GC-EPOCH*
            with gc-rt = sb-ext:*gc-run-time*
            for tx1 = (first (textures w))
            for tx2 = (second (textures w))
            for i fixnum = 0 then (ldb (byte 32 0) (1+ i))
            while (glop:dispatch-events w :blocking nil :on-foo nil)
            do
               (when (> (- (get-internal-real-time) last)
                        (* 4 internal-time-units-per-second))
                 (setf last (get-internal-real-time))
                 (format t "~s fps~%" (/ frames 4.0))
                 (force-output)
                 (setf frames 0))
               (multiple-value-bind (framep start end telemetry error)
                   (3b-lepton::read-frame)
                 (declare (ignorable start end telemetry framep error))
                 (when error
                   (format t "@~s:~s~%" i error)
                   (when (eq error :quit)
                     (loop-finish)))
                 (when start ;(eql end 120)
                   (incf frames)
                   #++(upload-frame w frame)
                   #++(upload-frame-part w frame start end)
                   #++(gl:bind-texture :texture-2d tx2)
                   (draw w)
                   #++(gl:bind-texture :texture-2d tx1)
                   ;;(upload-frame w frame)
                   #++(upload-frame-part w frame 0 120)
                   (upload-frame-part w frame start end)
                   #++(rotatef tx1 tx2)
                   (glop:swap-buffers w))
                 (unless (eq gc-epoch sb-kernel::*gc-epoch*)
                   (setf gc-epoch sb-kernel::*gc-epoch*)
                   (format t "gc, rt=~s~%" (- sb-ext:*gc-run-time* gc-rt))
                   (setf gc-rt sb-ext:*gc-run-time*))
)
               #++(swank::process-requests t))
          )))))

#++
(viewer)

#++
(threaded-viewer)


#++
(3b-lepton::with-lepton (l)
  (3b-lepton::reboot l))

#++(sb-posix:getenv "DISPLAY")
#++
(sb-posix:setenv "DISPLAY" ":0.0" 1)

#++
(3b-lepton::with-lepton (l :i2c "0" )
  (list (3b-lepton::status l)
        (3b-lepton::serial-number l)
        (3b-lepton::uptime l)
        (3b-lepton::aux-temp l)
        (3b-lepton::fpa-temp l)))
