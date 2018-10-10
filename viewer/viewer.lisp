#++ (ql:quickload '3b-lepton/viewer)
(in-package 3b-lepton/viewer)

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

(defmethod glop:on-event ((w viewer) (event glop:button-event))
  (format t "Button ~:[released~;pressed~]: ~S~%"
          (glop:pressed event) (glop:button event))
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
  (gl:bind-texture :texture-2d (car (textures w )))
  #++(gl:tex-image-2d :texture-2d 0
                      :luminance16 160 120 0 :luminance :unsigned-short
                      buffer)
  (cffi:with-pointer-to-vector-data (p buffer)
    (gl:tex-image-2d :texture-2d 0
                     :rgba 160 120 0 :rgba :unsigned-byte
                     p)))

(defun draw (w)
  (gl:clear-color (random 1.0) (random 1.0) (random 1.0) 1)
  (gl:clear :color-buffer)
  (gl:color 1 1 1)
  (gl:enable :texture-2d :blend)
  (gl:with-primitive :polygon
    (gl:tex-coord 0.0 1.0)
    (gl:vertex 0.02 0.02 0)
    (gl:tex-coord 1.0 1.0)
    (gl:vertex 0.97 0.02 0)
    (gl:tex-coord 1.0 0.0)
    (gl:vertex 0.97 0.97 0)
    (gl:tex-coord 0.0 0.0)
    (gl:vertex 0.02 0.97 0)))

(defun viewer ()
  (3b-lepton::with-lepton (lepton)
    (glop:with-window (w "3b-lepton" 640 480 :win-class 'viewer)
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
      (gl:bind-texture :texture-2d (car (textures w )))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (let ((3b-lepton::*rgb-lut* (copy-seq 3b-lepton::*rgb-lut*) ))
        (update-lut)
        (3b-lepton::with-capture (lepton
                                  frames-buffer
                                  :count 1
                                  :filter-generator '3b-lepton::sf-rgb-lut)
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
               (3b-lepton::read-frame)
               (upload-frame w frames-buffer)
               (draw w)
               (glop:swap-buffers w)
               (swank::process-requests t)))))))

#++
(viewer)


#++
(3b-lepton::with-lepton (l)
  (3b-lepton::reboot l))

#++(sb-posix:getenv "DISPLAY")
#++
(sb-posix:setenv "DISPLAY" ":0.0" 1)
