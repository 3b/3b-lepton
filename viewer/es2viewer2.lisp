#++ (ql:quickload '(3b-lepton/es2viewer2))
(in-package 3b-lepton/es2viewer2)

(setf %gl:*gl-get-proc-address* 'egl::get-proc-address)

(defclass viewer (glop2::window)
  ((lepton :initform nil :accessor lepton)
   (textures :initform nil :accessor textures)
   (programs :initform (make-hash-table) :reader programs)
   (vbo :initform nil :accessor vbo)
   (uniforms :initform nil :accessor uniforms)))

(defparameter *vs* "
attribute vec3 position;
attribute vec2 uv;

varying vec2 vuv;
uniform mat4 mvp;
void main () {
  vuv = uv;
  gl_Position = mvp * vec4(position,1.0);
}")

(defparameter *fs* "
precision mediump float;
varying vec2 vuv;
uniform sampler2D tex;
uniform sampler2D lut;
uniform float r;
void main () {
  float g = texture2D(tex,vuv).x * 256.0;
  vec2 uv2 = vec2(fract(g),floor(g)/256.0);
  vec4 c = texture2D(lut,uv2);
  c.a = 1.0;
  gl_FragColor = c;
}")


(defparameter *fs2* "
precision mediump float;
varying vec2 vuv;
uniform sampler2D lut;
void main () {
  float g = vuv.y * 256.0;
  vec2 uv2 = vec2(fract(g),floor(g)/256.0);
  vec4 c = texture2D(lut,uv2);
  c.a = 1.0;
  gl_FragColor = c;
}")

(defparameter *vbo*
  (map '(simple-array single-float (*))
       'float
       (let* ((x 0.95)
              (-x (- x)))
         (vector 0 1      -x -x 0
                 0 0      -x x 0
                 1 0      x x 0

                 0 1      -x -x 0
                 1 1      x -x 0
                 1 0      x x 0

                  ))))

(defparameter *dbgtex*
  (coerce #(0 #x6000 #xc000 #xffff
            0 0 #xc000 #xc000
            #x8000 #x8000 0 0
            #x8000 #x8000 0 0)
       '(simple-array (unsigned-byte 16) (*)))
  #++(coerce #(0 0 #xffff #xffff
            0 0 #xffff #xffff
            #x8000 #x8000 0 0
            #x8000 #x8000 0 0)
       '(simple-array (unsigned-byte 16) (*))))

(defun compile-shader (source type)
  (let ((s (gl:create-shader type)))
    (gl:shader-source s source)
    (gl:compile-shader s)
    (format t "compiled ~s: ~s~%" type (gl:get-shader-info-log s))
    (if (gl:get-shader s :compile-status)
        s
        (progn (gl:delete-shader s) nil))))

(defun compile-program (v f attrib-loc attrib-name &rest more-attribs)
  (let ((s1 (compile-shader v :vertex-shader))
        (s2 (compile-shader f :fragment-shader))
        (p (gl:create-program))
        r)
    (unwind-protect
         (when (and s1 s2)
           (gl:attach-shader p s1)
           (gl:attach-shader p s2)
           (loop for (l n) on (list* attrib-loc attrib-name more-attribs)
                 by #'cddr
                 do (gl:bind-attrib-location p l n))
           (gl:link-program p)
           (format t "linked: ~s~%" (gl:get-program-info-log p))
           (when (gl:get-program p :link-status)
             (setf r p)))
      (when s1 (gl:delete-shader s1))
      (when s2 (gl:delete-shader s2))
      (unless r (gl:delete-program p)))
    r))

(defun cleanup (w)
  (when (textures w)
    (gl:delete-textures (shiftf (textures w) nil)))
  (mapcar 'gl:delete-program (prog1 (mapcar 'car
                                            (alexandria:hash-table-values
                                             (programs w)))
                               (clrhash (programs w))))
  (when (vbo w) (gl:delete-buffers (list (shiftf (vbo w) nil))))
  (setf (uniforms w) nil))

(defun lut0 (i)
  (let* ((r (ldb (byte 8 6) i))
         (g (ldb (byte 8 3) i))
         (b (ldb (byte 8 0) i)))
    (logior (ash r 16) (ash g 8) b)))

(defun lut1 (x)
  (let* ((g (floor (* 255 0.25 (+ 2 (sin (* x (/ pi 4096)))
                                  (cos (* x (/ pi 512)))))))
         (b (floor (* 255 0.5 (1+ (sin (* (+ x 1234) (/ pi 2048)))))))
         (r (floor (* 255 0.5 (1+ (cos (* x (/ pi 1024))))))))
    (logior (ash b 16) (ash g 8) (ash r 0) ;255
            )))

(defun lut2 (x)
  (let* ((a (+ 0.2 (* 0.6 (/ x 65536.0))))
         (g (floor (* a 255 0.25 (+ 2 (sin (* x (/ pi 14096)))
                                    (cos (* x (/ pi 512)))))))
         (b (floor (* a 255 0.5 (1+ (sin (* (+ x 1234)
                                            (/ pi 8048)))))))
         (r (floor (* a 255 0.5 (1+ (cos (* x (/ pi 256))))))))
    (logior (ash b 16) (ash g 8) (ash r 0) ;255
            )))

(defparameter *lut* 'lut1)

(defun upload-lut (w generator)
  (gl:active-texture 1)
  (gl:bind-texture :texture-2d (second (textures w )))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (cffi:with-foreign-object (p :unsigned-int 65536)
    (loop for i below 65536
          do (setf (cffi:mem-aref p :unsigned-int i)
                   (funcall generator i)))
    (gl:tex-image-2d :texture-2d 0 :rgba 256 256 0 :rgba
                     :unsigned-byte p)))




(defun setup (w)
  (gl:disable :cull-face)
  #++(glop:swap-interval w 0)
  (gl:clear-color 0.3 0.3 0.3 0)
  (when (textures w)
    (gl:delete-textures (shiftf (textures w) nil)))
  (setf (textures w)
        (gl:gen-textures 2))
  (upload-lut w *lut*)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d (first (textures w)))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  #++(gl:enable :texture-2d)
  #++(gl:tex-image-2d :texture-2d 0 :depth-component 4 4 0 :depth-component
                      :unsigned-short *dbgtex*)
  (gl:tex-image-2d :texture-2d 0 :depth-component 160 120 0 :depth-component
                   :unsigned-short nil)
  #++(gl:tex-image-2d :texture-2d 0 :rgba 160 120 0 :rgba
                      :unsigned-byte nil)
#++
  (let ((p (compile-program *vs* *fs*
                            0 "position"
                            1 "uv")))
    (when p
      (when (gethash :lut (programs w))
        (gl:delete-program (car (shiftf (gethash :lut (programs w)) nil))))
      (gl:use-program p)
      (setf (gethash :lut (programs w))
            (list p
                  :tex (gl:get-uniform-location p "tex")
                  :lut (gl:get-uniform-location p "lut")
                  :mvp (gl:get-uniform-location p "mvp")
                  :r (gl:get-uniform-location p "r")))
      (destructuring-bind (&key tex lut mvp r)
          (cdr (gethash :lut (programs w)))
        (declare (ignorable r))
        (format t "uniforms = ~s~%" (uniforms w))
        (gl:uniformi tex 0)
        (gl:uniformi lut 1)
        (gl:uniform-matrix mvp 4 #(#(1 0 0 0
                                     0 1 0 0
                                     0 0 1 0
                                     0 0 0 1))
                           nil))))
  (macrolet ((update-program (key
                              (vs fs &rest attribs)
                              (&rest uniform-vars)
                              &body body)
               (let ((u (loop for v in uniform-vars
                              collect
                              (etypecase v
                                ((cons cons)
                                 (list (caar v) (cadar v) (cadr v)))
                                (cons
                                 (list (intern (symbol-name (first v))
                                               :keyword)
                                       (first v)
                                       (second v)))
                                (symbol
                                 (list (intern (symbol-name v) :keyword)
                                       v
                                       (format nil "~(~a~)" v))))))
                     (p (gensym)))
                 `(let ((,p (compile-program ,vs ,fs ,@attribs)))
                    (when ,p
                      (when (gethash ,key (programs w))
                        (gl:delete-program (car
                                            (shiftf (gethash ,key (programs w))
                                                    nil))))
                      (gl:use-program ,p)
                      (setf (gethash ,key (programs w))
                            (list ,p
                                  ,@(loop
                                      for (k nil s) in u
                                      collect k
                                      collect
                                      `(gl:get-uniform-location ,p ,s))))
                      (destructuring-bind (&key ,@ (mapcar 'second u))
                          (cdr (gethash ,key (programs w)))
                        (declare (ignorable ,@ (mapcar 'second u)))
                        (format t "uniforms = ~s~%"
                                (cdr (gethash ,key (programs w))))
                        ,@body))))))
    (update-program :lut
                    (*vs* *fs* 0 "position" 1 "uv")
                    (tex lut mvp r)
                    (gl:uniformi tex 0)
                    (gl:uniformi lut 1)
                    (gl:uniform-matrix mvp 4 #(#(1 0 0 0
                                                 0 1 0 0
                                                 0 0 1 0
                                     0 0 0 1))
                           nil))
    (update-program :grad
                    (*vs* *fs2* 0 "position" 1 "uv")
                    (lut mvp)
                    (gl:uniformi lut 1)
                    (gl:uniform-matrix mvp 4 #(#(1 0 0 0
                                                 0 1 0 0
                                                 0 0 1 0
                                                 0 0 0 1))
                                       nil)))
  (format t "programs = ~s~%" (alexandria:hash-table-plist (programs w)))
  (gl:use-program (car (gethash :lut (programs w))))

  (let ((b (gl:gen-buffer)))
    (%gl:bind-buffer :array-buffer b)
    (cffi:with-pointer-to-vector-data (p *vbo*)
      (%gl:buffer-data :array-buffer (* 4 (length *vbo*)) p :static-draw)
      (gl:enable-vertex-attrib-array 0)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 0 3 :float nil 20 8)
      (gl:vertex-attrib-pointer 1 2 :float nil 20 0))
    (when (vbo w) (gl:delete-buffers (list (shiftf (vbo w) nil))))
    (setf (vbo w) b)))


(defmethod glop2:on-key ((w viewer)  pressed code sym text)
  (format t "Key ~:[released~;pressed~]: ~A~%" pressed sym)
  (when (eq sym :escape)
    (glop2:push-close-event w)))

(defparameter *fullscreen* nil)
(defparameter *mp* (vector 0 0 0 0))
(defparameter *saving* nil)
(defmethod glop2:on-button ((w viewer) pressed button)
  (format t "Button ~:[released~;pressed~]: ~S~%" pressed button)
  ;; (setup w)
  (unless pressed
    (destructuring-bind (x y dx dy) (coerce *mp* 'list)
      (declare (ignorable x y dx dy))
      (let ((wx (glop2:width w))
            (wy (glop2:height w)))
        (declare (ignorable wx wy))
        (cond
          ;; upper left
          ((and (< x (* wx 1/4))
                (< y (* wy 1/4)))
           (glop2:push-close-event w))
          ;; lower left
          ((and (< x (* wx 1/4))
                (> y (* wy 3/4)))
           #++(egl:swap-interval (glop2::egl-display w) 0)
           #++(glop:set-geometry w 1 0 800 480))
          ;; upper right
          ((and (> x (* wx 3/4))
                (< y (* wy 1/4)))
           (let ((p (format nil "~~/img/~a/" (get-universal-time))))
             (format t "Save to ~s~%" p)
             (setf *saving*
                   (list (ensure-directories-exist p)
                         45
                         0))))
          ;; lower right
          ((and (> x (* wx 3/4))
                (> y (* wy 3/4)))
           #++(glop:set-geometry w 0 0 800 480))
          ;; left mid
          ((< x (/ wx 2))
           (setup w))

         (t
          (format t "~s~%"
                  (gl:get-string :extensions))
          (setf *fullscreen* (not *fullscreen*))
          (format t "fs -> ~s~%" *fullscreen*)
          #++(glop:set-fullscreen w *fullscreen*))))))

  #++(update-lut))

(defmethod glop2:on-mouse-motion ((w viewer) x y dx dy)
  (setf (aref *mp* 0) x
        (aref *mp* 1) y
        (aref *mp* 2) dx
        (aref *mp* 3) dy)
  (format t "mm ~s~%" *mp*))

(defmethod glop2:on-touch-down ((w viewer) slot x y)
  (setf (aref *mp* 0) x
        (aref *mp* 1) y)
  (setf *flash* (not *flash*))
  (if *flash*
      (Setf *saving* nil)
      (let ((p (format nil "~~/img/~a/" (get-universal-time))))
        (format t "Save to ~s~%" p)
        (setf *saving*
              (list (ensure-directories-exist p)
                    nil ;; record until manually stopped
                    0))))
  (format t "td ~s ~s~%" slot *mp*))

(defmethod glop2:on-touch-motion ((w viewer) slot x y)

  )

(defmethod glop2:on-touch-up ((w viewer) slot)
  (format t "up ~s~%" slot))
(defmethod glop2:on-touch-frame ((w viewer))
  (format t "tframe~%"))


(defmethod glop2:on-resize ((w viewer) x y width height)
  (gl:viewport 0 0 width height)
  (format t "Resize: ~Sx~S~%" width height))

(defmethod glop2:on-visibility ((w viewer) visible)
  (format t "expose event ~s~%" visible)
  )

(defmethod glop2:on-close ((w viewer))
  )


#++
(defun upload-frame (w buffer)
  #++
  (cffi:with-pointer-to-vector-data (p buffer)
    (gl:tex-image-2d :texture-2d 0
                     :rgba 160 120 0 :rgba :unsigned-byte
                     p)))



(defun upload-frame-part (w buffer y1 y2)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d (first (textures w )))
  (cffi:with-pointer-to-vector-data (p buffer)
    (gl:tex-sub-image-2d :texture-2d 0
                         0 y1 160 (- y2 y1) :depth-component :unsigned-short
                         (cffi:inc-pointer p (* y1 160 2)))))

(defvar *w* nil)
(defparameter *fooo* 0.0)
(defparameter *thunk* nil)
#++
(setf *thunk*
      (lambda ()
        (format t "~s~%"
                (glop2/backend-drm::foo))))

(defvar *flash* t)
(defun now ()
  (/ (get-internal-real-time)
     (float internal-time-units-per-second)))
(defun draw (w)
  (when *thunk*
    (with-simple-restart (continue  "continue")
      (funcall (shiftf *thunk* nil))))
  (setf *w* w)
  (if *flash*
      (gl:clear-color (random 0.8) (random 0.2) (random 0.2) 1)
      (let ((n (mod (now) 1000.0)))
        (flet ((c (x) (1+ (* 0.5 x))))
         (gl:clear-color (c (sin (* n 4))) (c (cos (* n 3))) (c (sin (* n 2))) 1))))
  #++(gl:clear :color-buffer :depth-buffer)
  ;;(gl:clear :depth-buffer)
  (gl:clear :color-buffer)
  (when (and (programs w) (textures w) (vbo w))
    (when (gethash :lut (programs w))
      (gl:use-program (car (gethash :lut (programs w))))
      (gl:draw-arrays :triangles 0 6))
    (when (gethash :grad (programs w))
      (destructuring-bind (p &key mvp &allow-other-keys)
          (gethash :grad (programs w))
        (gl:use-program p)
        (gl:uniform-matrix mvp 4 #(#(0.1 0 0 0
                                     0 1 0 0
                                     0 0 1 0
                                     0.95 0 0 1))
                           nil)
      (gl:draw-arrays :triangles 0 6)))
    )
  (glop2:swap-buffers w)
  ;(sleep 0.1)
  #++
  (cffi:with-foreign-object (r 'egl::eglint 4) ; x y w h
    (setf (cffi:mem-aref r 'egl::eglint 0) 0)
    (setf (cffi:mem-aref r 'egl::eglint 1) 0)
    (setf (cffi:mem-aref r 'egl::eglint 2) 32)
    (setf (cffi:mem-aref r 'egl::eglint 3) 32)
    (egl::eglswapbufferswithdamagekhr (glop::egl-display w)
                                      (glop::egl-surface w)
                                      r 1)))

#++
(defun viewer ()
  (unless (sb-posix:getenv "DISPLAY")
    (sb-posix:setenv "DISPLAY" ":0.0" 1))
  (3b-lepton::with-lepton (lepton :i2c "0")
    (glop:with-window (w "3b-lepton" 800 (- 480 0)
                         :win-class 'viewer :x 0 :y 0
                         :alpha-size 8)
      (setf (lepton w) lepton)
      (setup w)
      (let ((3b-lepton::*rgb-lut* (copy-seq 3b-lepton::*rgb-lut*) ))
        #++(update-lut)
        (format t "capture~%")
        (3b-lepton::with-capture (lepton
                                  frames-buffer
                                  :count 1
                                  :filter-generator '3b-lepton::pf-rgb-lut)
          (format t "upload1~%")
          (upload-frame w frames-buffer)
          (format t "loop~%")
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
               (upload-frame-part w frames-buffer 0 120)
               (draw w)))))))


(defparameter *exit* t)
#++
(progn (setf *exit* t)
       (setf *saving* nil)
       (sleep 1)
       (threaded-viewer))

(defun threaded-viewer ()
  #++(unless (sb-posix:getenv "DISPLAY")
       (sb-posix:setenv "DISPLAY" ":0.0" 1))
  (glop2:with-application (:3d-api :gles)
    (3b-lepton::with-lepton (lepton :i2c "0")
      (let ((l lepton))
        (print
         (list (3b-lepton::status l)
               (3b-lepton::serial-number l)
               (3b-lepton::uptime l)
               :agc (3b-lepton::agc-enabled l)
               :gain (3b-lepton::gain-mode l)
               :t-lin (3b-lepton::t-linear l)
               (3b-lepton::t-linear-resolution l)
               (3b-lepton::t-linear-auto-resolution l)
               :vid (3b-lepton::vid-output-format l)
               :rad (3b-lepton::radiometry-control l)
               :spot (3b-lepton::spotmeter-roi l) (3b-lepton::spotmeter-value l )
               :aux (3b-lepton::aux-temp l)
               :fpa (3b-lepton::fpa-temp l))))
      (let ((w (make-instance 'viewer)))
        (glop2:open-window  w "3b-lepton" 800 400
                            :events '(glop2:on-touch-down
                                      glop2:on-touch-up
                                      glop2:on-touch-motion
                                      glop2:on-touch-frame
                                      glop2:on-close
                                      glop2:on-resize))
        (setf (lepton w) lepton)
        (setup w)
        (let ((3b-lepton::*rgb-lut* (copy-seq 3b-lepton::*rgb-lut*)))
          #++(update-lut)
          (setf *exit* nil)
          (3b-lepton::with-reader-thread (lepton frame
                                          :filter-generator 3b-lepton::sf-raw)
            (loop
              with *print-length* = 16
              with last = (get-internal-real-time)
              with frames = 0
              with gc-epoch = SB-KERNEL::*GC-EPOCH*
              with gc-rt = sb-ext:*gc-run-time*
              for tx1 = (first (textures w))
              for tx2 = (second (textures w))
              for i fixnum = 0 then (ldb (byte 32 0) (1+ i))
              while (and (glop2:dispatch-events) (not *exit*))
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
                   (when (and start (not error))          ;(eql end 120)
                     #++(upload-frame w frame)
                     (upload-frame-part w frame start end)
                     (when (eql end 120)
                       (when *saving*
                         (unless (third *saving*)
                           (setf (third *saving*) 0))
                         (with-open-file (f (format nil "~a/~a"
                                                    (first *saving*)
                                                    (third *saving*))
                                            :element-type '(unsigned-byte 16)
                                            :direction :output
                                            :if-does-not-exist :create)
                           (write-sequence frame f))
                         (incf (third *saving*))
                         (when (and (second *saving*)
                                    (>= (third *saving*) (second *saving*)))
                           (format t "save done~%")
                           (setf *saving* nil)))
                       (incf frames)
                       (with-simple-restart (:continue "continue")
                         (draw w))))
                   (unless (eq gc-epoch sb-kernel::*gc-epoch*)
                     (setf gc-epoch sb-kernel::*gc-epoch*)
                     (format t "gc, rt=~s~%" (- sb-ext:*gc-run-time* gc-rt))
                     (setf gc-rt sb-ext:*gc-run-time*)))
              finally (3b-lepton::exit-reader))))))))

#++
(viewer)

#++
(ql:quickload '(3b-lepton/es2viewer2))
#++
(threaded-viewer)
#++
(setf *exit* t)
#++(setf *saving* nil)
#++
(let ((p (format nil "~~/img/~a/" (get-universal-time))))
  (format t "Save to ~s~%" p)
  (setf *saving*
        (list (ensure-directories-exist p)
              900
              0)))

#++
(3b-lepton::with-lepton (l)
  (3b-lepton::reboot l))

#++(sb-posix:getenv "DISPLAY")
#++
(sb-posix:setenv "DISPLAY" ":0.0" 1)

#++
(3b-lepton::with-lepton (l :i2c "0" )
  (print
   (list (3b-lepton::status l)
         (3b-lepton::serial-number l)
         (3b-lepton::uptime l)
         :agc (3b-lepton::agc-enabled l)
         :gain (3b-lepton::gain-mode l)
         :t-lin (3b-lepton::t-linear l)
         (3b-lepton::t-linear-resolution l)
         (3b-lepton::t-linear-auto-resolution l)
         :vid (3b-lepton::vid-output-format l)
         :rad (3b-lepton::radiometry-control l)
         :spot (3b-lepton::spotmeter-roi l) (3b-lepton::spotmeter-value l )

         :aux (3b-lepton::aux-temp l)
         :fpa (3b-lepton::fpa-temp l))))
