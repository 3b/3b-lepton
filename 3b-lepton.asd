
(asdf:defsystem 3b-lepton
  :description "Common Lisp library for using FLIR Lepton thermal camera over i2c/spi"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (cl-spidev cffi alexandria 3b-i2c bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "lep-error")
               (:file "lepton")
               (:file "command")
               (:file "capture")
               (:file "capture-thread")))

(asdf:defsystem 3b-lepton/viewer
  :depends-on (3b-lepton glop cl-opengl)
  :serial t
  :components ((:module
                "viewer"
                :components ((:file "package")
                             (:file "viewer")))))


(asdf:defsystem 3b-lepton/es2viewer
  :depends-on (3b-lepton glop/egl cl-opengl/es2)
  :serial t
  :components ((:module
                "viewer"
                :components ((:file "package")
                             (:file "es2viewer")))))

(asdf:defsystem 3b-lepton/es2viewer2
  :depends-on (3b-lepton cl-opengl/es2 glop2/drm+egl)
  :serial t
  :components ((:module
                "viewer"
                :components ((:file "package")
                             (:file "es2viewer2")))))
