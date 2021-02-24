(ql:quickload '(3b-lepton/es2viewer2 swank sb-sprof) )
(swank:swank-require 'swank-repl)
(setf swank:*globally-redirect-io* t)

(defvar *quit* t)
(sb-ext:save-lisp-and-die
 #p"~/bin/foo"
 :executable t
 :toplevel #++ #'3b-lepton/es2viewer2::threaded-viewer
 (lambda ()
   (setf *quit* nil)

   (format *standard-output* "testing *standard-output*...~%")
   (format sb-sys:*stdout* "testing stdout...~%")
   (format sb-sys:*stderr* "testing stderr...~%")

   (swank:create-server :port 40005 :dont-close t)

   (format *standard-output* "testing *standard-output*...~%")
   (format sb-sys:*stdout* "testing stdout...~%")
   (format sb-sys:*stderr* "testing stderr...~%")

   #++
   (let ((log (open "/tmp/log" :direction :output
                               :if-does-not-exist :create
                               :if-exists :append)))
     (setf *standard-output* (make-broadcast-stream
                              log *standard-output*))
     (setf *error-output* (make-broadcast-stream
                           log *error-output*)))
   (setf *standard-output* (make-broadcast-stream))
   ;;(setf *error-output* (make-broadcast-stream))
   #++(3b-lepton/es2viewer2::threaded-viewer)
   (sleep 100)
   (when swank::*connections*
     (loop until *quit*
           do (sleep 1)))))

